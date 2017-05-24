%% @author jvsingh
%% @doc  utility functions -  to deal with the \n
%% @doc 		Erlang digraph data structure. 
%% @doc API's in this module deal with graphs, vertices and edges 
%% 		


-module(graph_utilities).

%% ====================================================================
%% API functions
%% ====================================================================
-export([member/2, get_existing_edge/3, get_existing_edge/4,
		  create_update_edge/4, get_connected_vertices/3, 
		   serialize/1, deserialize/1]).

-include("graph_types.hrl").

%% @doc returns true if an element is a member of the list
%%-spec member(Element::term(),List::list()) -> true | false.
member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H, T);
member(_,[]) ->	false.


%% @doc returns an existing edge from vertex1 to vertex2
%% From the main graph, it creates a subgraph using the two vertices to 
%% extract only the edges between the two. (digraph:get_path won't work here)
-spec get_existing_edge(Graph::digraph:digraph(), 
						Vertex1::digraph:vertex(), 
						Vertex2::digraph:vertex()) -> {ok, digraph:edge()}.
get_existing_edge (Graph, Vertex1, Vertex2)->
	SubGraph = digraph_utils:subgraph(Graph,[Vertex1, Vertex2]),
	[H|_Empty] = digraph:in_edges(SubGraph, Vertex2),
	H.

%% @doc return an edge from Vertex1 to Vertex2 with a specific label
-spec get_existing_edge(digraph:digraph(), 
							 digraph:vertex(), 
							 digraph:vertex(),
							 digraph:label()) -> digraph:vertex().
get_existing_edge(Graph, Vertex1, Vertex2, Label) ->
	[H|_] = lists:umerge(ets:match(Graph#digraph.etab, 
								   {'$1', Vertex1,Vertex2, Label})),
	H.

%% @doc create or update an edge from Vertex1 and Vertex2 with a Label
create_update_edge(Graph,Vertex1,Vertex2, Label) ->
	SubGraph = digraph_utils:subgraph(Graph,[Vertex1, Vertex2]),
	case (digraph:in_degree(SubGraph, Vertex2) > 0) of 
		true -> get_existing_edge(Graph, Vertex1,Vertex2);
		false -> digraph:add_edge(Graph, Vertex1, Vertex2, Label)
	end.



%% @doc return a list of connected vertices with the labeled edge
-spec get_connected_vertices(digraph:digraph(), 
							 digraph:vertex(), 
							 digraph:label()) -> [digraph:vertex()].
get_connected_vertices(Graph, Vertex, Label) ->
	lists:umerge(ets:match(Graph#digraph.etab, {'_', Vertex, '$2', Label})).

get_all_connected_vertices(Graph, Vertex, Label) ->
	lists:umerge(ets:match(Graph#digraph.etab, {'_', Vertex, '$2', Label}) 
				++ 
				ets:match(Graph#digraph.etab, {'_','$2',Vertex, Label})).

%% @doc serialise the contents of the underlying ETS tables of the digraph
%% (the solution to serialize and deserialize the digraph is based on this: 
%%	http://stackoverflow.com/questions/15552796/how-to-pass-a-digraph-to-a-different-process-and-node)
serialize({digraph, V, E, N, B}) ->
    {ets:tab2list(V),
     ets:tab2list(E),
     ets:tab2list(N),
     B}.

deserialize({VL, EL, NL, _B}) ->       
    Digraph = {digraph, V, E, N, true} = digraph:new(),
    ets:delete_all_objects(V),
	ets:delete_all_objects(E),
    ets:delete_all_objects(N),
    ets:insert(V, VL),
    ets:insert(E, EL),
    ets:insert(N, NL),
    Digraph.

%% ====================================================================
%% Internal functions
%% ====================================================================
