%% @author jvsingh
%% @doc @todo A singleton Graph server to store nodes, 
%% @doc		  bi-directional connections and attach attributes to nodes

-module(graph_server).
-behaviour(gen_server).  %%@todo implement full gen_server behaviour

%% Client functions
-export([start_link/0]).
-export([stop/0, add_node/2, delete_node/1, 
		 add_connection/2, 
		 add_attribute/2,
		 delete_attribute/2,
		 delete_connection/2, 
		 get_attributes/1,
		 get_connections/1,
		 get_attribute/3,
		 get_server_data/0]).

-export([is_connected/2,
		 get_connections/2]).

%% Internal exports
-export([init/0]).

-type error_reason() :: list().
-type attribute_type() :: term().
-type node_type() :: {digraph:vertex()}.
-type link_graph() :: {digraph:graph()}.

%% @doc start server
-spec start_link() -> {ok, pid()} | {error, error_reason()}.
start_link() -> 
	Pid=spawn(graph_server, init, []),
	try
		register(graph_server, Pid),
		{ok, Pid}
	catch
		error:Reason -> {error,Reason}
	end.
	

%% @doc Initialises the graph data from a persistent store (e.g. a File).
%% 		If it can't read from the file, 
%%		then a new digraph instance is created for the server
init() -> 	
	try
		{ok, [GraphData]} = file:consult("graph_server.dat"), 
		ExistingGraph = graph_utilities:deserialize(GraphData),
		loop({ExistingGraph})
	catch
		error:_Reason -> 
		Graph = digraph:new(),
		loop({Graph})
	end.
	

-spec loop(LinkNetGraph::link_graph()) -> ok.
loop({Graph}) ->	
	receive
		{FromPid, Ref, {add_node, Node, AttributeList}} ->
			%% a "Node" vertex is created with label node
			digraph:add_vertex(Graph, Node, node),
			do_add_attributes(Graph, Node, AttributeList),
			FromPid!{Ref, ok},
			do_write_data(Graph), 
			loop({Graph});
		{FromPid, Ref, {delete_node, Node} }->
			true = digraph:del_vertex(Graph,Node),
			FromPid!{Ref, ok},
			do_write_data(Graph), 
			loop({Graph});
		{FromPid, Ref, {add_connection, Node1, Node2}} ->
			FromPid!{Ref, do_add_connection(Graph, Node1, Node2)},
			do_write_data(Graph), 
			loop({Graph});
		
		{FromPid, Ref, {delete_connection, Node1, Node2}} ->
			FromPid!{Ref, do_delete_connection(Graph, Node1, Node2)},
			do_write_data(Graph), 
			loop({Graph});
		
		{FromPid, Ref, {get_connections,Node} } ->
			FromPid!{Ref, {ok,digraph:in_neighbours(Graph, Node)} },
			loop({Graph});
			
		
		{FromPid, Ref, {add_attribute, Node, Attribute}} ->
			FromPid!{Ref, do_add_attribute(Graph, Node, Attribute)},
			do_write_data(Graph), 
			loop({Graph});
		
		{FromPid, Ref, {delete_attribute, Node, Attribute}} ->
			FromPid!{Ref, do_delete_attribute(Graph, Node, Attribute)},
			do_write_data(Graph), 
			loop({Graph});
		
		{FromPid, Ref, {get_attributes, Node}} ->
			FromPid!{Ref, {ok,
						     graph_utilities:get_connected_vertices(
							         Graph,
									 Node,
									 attribute_edge)}},
			loop({Graph});
		
		{stop, FromPid} -> 
			do_write_data(Graph),
			FromPid!{ok, self()};
		
		{print, FromPid} -> 
			FromPid!digraph_utils:preorder(Graph),
			loop({Graph});
		{get_server_data, FromPid} -> 
			FromPid ! {ok,self(),graph_utilities:serialize(Graph)},
			loop({Graph});
		
		{FromPid, Ref,{is_connected,Node1,Node2}} ->
			    GetPathResponse = digraph:get_path(Graph, Node1, Node2),
			 	FromPid !{Ref, do_prepare_response(GetPathResponse)},
				loop({Graph});
		{FromPid, Ref, {get_connections, Node, Degree}} ->
				ConnectionList = bfs:bfs(Graph,Node, Degree),
				FromPid!{Ref,{ok,ConnectionList}},
				loop({Graph});
		
		{FromPid, Ref, {get_attribute, Node, Attribute, Degree}} ->
			FromPid!{Ref, do_get_attribute(Graph,Node, Attribute, Degree)},
			loop({Graph});
		
		RandomMessage ->
			io:format("Unknown Message: ~n\t~p~n", [RandomMessage]),
			loop({Graph})
	end.
 

%% @doc stop the link database server. 
-spec stop() -> ok.
stop() -> 
	graph_server ! {stop, self()},
	ServerPid = whereis(graph_server),
	receive
		{ok, ServerPid} -> ok
	end. 

%% @doc add a node to the server - as yet they have no connections
-spec add_node(Node::node_type(), [attribute_type()]) -> ok.
add_node(Node, AttributeList) -> 
	request({add_node, Node, AttributeList}).


%% @doc Remove a node from the server and remove all connections to that node.
-spec delete_node(node_type()) -> ok | {error, error_reason()}.
delete_node(Node) -> 
	request({delete_node, Node}).


%% @doc Add a connection from node Node1 to node Node2 if both exist
-spec add_connection(node_type(), node_type()) -> 
		  								ok | {error, error_reason()}.
add_connection(Node1, Node2) -> 
	request({add_connection, Node1, Node2}).
	

%% @doc Delete the connection between Node1 to Node2. 
%%		We are assuming both people exist.
-spec delete_connection(node_type(), node_type()) -> 
		  								ok | {error,error_reason()}.
delete_connection(Node1, Node2) -> 
	request({delete_connection, Node1, Node2}).

%% @doc Return the possibly empty NodeList of people connected to Node.
-spec get_connections(node_type()) -> 
		  					{ok, [node_type()]} | {error,error_reason()}.
get_connections(Node) -> 
	request({get_connections, Node}).

%% @doc Add an Attribute to node Node. We are assuming the node exists.
-spec add_attribute(node_type(), attribute_type()) -> 
		  							ok | {error, error_reason()}.
add_attribute(Node, Attribute) ->
	request({add_attribute, Node, Attribute}).


%% @doc Delete the Attribute from Node. We are assuming the node exists.
-spec delete_attribute(node_type(), attribute_type()) -> 
		  							ok | {error, error_reason()}. 
delete_attribute(Node, Attribute) -> 
	request({delete_attribute, Node, Attribute}).


%% @doc Return the possibly empty AttributeList of Node.
-spec get_attributes(node_type()) -> {ok, [attribute_type()]} | {error,error_reason()}. 
get_attributes(Node) -> 
	request({get_attributes, Node}).
	
	
get_server_data() ->
	graph_server!{get_server_data, self()},
	ServerPid = whereis(graph_server),
	receive
		{ok, ServerPid, ServerData} -> {ok,ServerData}
	end.

%% ----Additional functions added to the API after logic moved to the server
%% @doc Is Node1 connected to Node2. If so, return the sequence of people in 
%%			the shortest connection, else no. 
-spec is_connected(Node1::node_type(), Node2::node_type()) -> 
		  								{yes, NodeList::[node_type()]} | no.
is_connected(Node1, Node2) -> 
	request({is_connected, Node1, Node2}).

%% @doc Get all the people which Node1 is directly connected to within Degree
%% 			of separation 
-spec get_connections(Node1::node_type(), 
					  Degree::term()) -> 
		  								{ok, NodeList::[node_type()]}.
get_connections(Node1, Degree) -> 
	request({get_connections, Node1, Degree}).

%% @doc Return the NodeList with Attribute within the Degree of separation
-spec get_attribute(Node::node_type(), Attribute::attribute_type(), Degree::term()) -> 
		  				{ok, NodeList::[node_type()]} | {error, Reason::term()}.
get_attribute(Node, Attribute, Degree) -> 
	request({get_attribute, Node, Attribute, Degree}).

%% ====================================================================
%% Internal functions
%% ====================================================================

%% @doc common function to make synchronous calls to the local server
-spec request(Message::term()) -> Reply::term().
request(Message) ->
	Ref = make_ref(), 
	?MODULE !{self(), Ref, Message}, 
	receive
		{Ref, Reply} -> Reply
	after 5000 ->
		io:format("Operation timed out for message: ~n\t~p~n", [Message]), 
		{error, timeout}
	end.



-spec do_add_attributes(Graph::link_graph(), node_type(), [attribute_type()]) -> ok.
do_add_attributes(_Graph, _Node, []) -> ok;
do_add_attributes(Graph, Node, [Attribute|RestOfAttributeList]) ->
	digraph:add_vertex(Graph, Attribute, attribute),
	graph_utilities:create_update_edge(Graph, Node, Attribute, attribute_edge),
	do_add_attributes(Graph,Node,RestOfAttributeList).

%% @doc function that formats responses of type {yes, Data} | no
do_prepare_response(false) ->
	no;
do_prepare_response(Response) ->
	{yes, Response}.


%% @doc adds edges labeled connection_edge between Node1 and Node2 
%% @doc		in both directions
do_add_connection(Graph, Node1, Node2) ->
	%% connection_edge is added in both directions; avoid multiple 
	try
		['$e'|_Edge] = graph_utilities:create_update_edge(Graph,
												Node1,
												Node2,
											connection_edge),
		['$e'|_ReverseEdge] = graph_utilities:create_update_edge(Graph,
													Node2,
													Node1,
											connection_edge),
		ok
	catch error:Reason ->	
		{error, Reason}
	end.

do_delete_connection(Graph, Node1, Node2) ->
	%%in_neighbours is simpler as only Node vertices can be in_neighbours.
    %% an alternative would be to query vertices connected by edges labeled 
	%%	"connection_edge"
	ConnectionsList = digraph:in_neighbours(Graph, Node1),
	IsMember = graph_utilities:member(Node2, ConnectionsList),
	case IsMember of
		true -> 
			SubGraph = digraph_utils:subgraph(Graph,[Node1, Node2]),
			Edges = digraph:in_edges(SubGraph, Node1) 
								++ digraph:in_edges(SubGraph, Node2),
			digraph:del_edges(Graph, Edges),
			ok;
		false -> {error,{noconnectionbetween, Node1, Node2}}
	end.

do_add_attribute(Graph, Node, Attribute) ->
	try
		digraph:add_vertex(Graph,Attribute,attribute),
		['$e'|_NewEdge] = graph_utilities:create_update_edge(Graph,
												Node,
												Attribute,
												attribute_edge),
		ok
	catch error:Reason ->
		{error,Reason}	
	end.

do_delete_attribute(Graph, Node, Attribute) ->
	try
		AttributeEdge = graph_utilities:get_existing_edge(Graph,
												Node,
												Attribute,
												attribute_edge),
		digraph:del_edge(Graph, AttributeEdge ),
		ok
	catch error:Reason ->
		{error, Reason}		
	end.

%% return a list of nodes having this Attribute
%%        starting from Node and up to distance Depth 
do_get_attribute(Graph, Node, Attribute, Depth) -> 
	%% nodes with this Attribute
	NodesWithAttribute = digraph:in_neighbours(Graph, Attribute),
	NodesWithinDegree = bfs:bfs(Graph, Node, Depth),
	NodesSet = sets:intersection(sets:from_list(NodesWithAttribute),
					   sets:from_list(NodesWithinDegree)),
	{ok, sets:to_list(NodesSet)}.


do_write_data(Graph) ->
	SerializedGraph = graph_utilities:serialize(Graph),
	file:write_file("graph_server.dat",io_lib:fwrite("~p.\n", [SerializedGraph]) ),
	ok.