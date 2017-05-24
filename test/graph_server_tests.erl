%% @author jvsingh
%% @doc @todo Add description to graph_server_tests.

-module(graph_server_tests).
-include_lib("eunit/include/eunit.hrl").

%% @todo move this to proper setup/teardown routine
start_test_() -> 	
	{ServerStatus, _Pid} = graph_server:start_link(),
	?_assertEqual(ok, ok). %% @todo 


add_node_test_() ->
	graph_server:add_node({"node1"}, [attribute1,attribute2]),
	?_assert(graph_server:add_node({"node2"}, [attribute2, attribute3]) =:= ok).

delete_node_test_() ->
	graph_server:add_node({"node1"}, [attribute1,attribute2]),
	?_assert(graph_server:delete_node({"node1"}) =:= ok).
delete_node_nonexistent_test_() ->
	?_assert(graph_server:delete_node("nonexistentnode1") =:= ok).

add_connection_test_() ->
	graph_server:add_node({"node1"}, [attribute1, attribute2]),
	graph_server:add_node({"node2"}, [attribute2, attribute3]),
	?_assert(graph_server:add_connection({"node1"}, {"node2"}) =:= ok).

add_connection_error_test_() ->
	graph_server:add_node({"nodeX1"}, [attribute1, attribute2]),
	graph_server:add_node({"nodeX2"}, [attribute2, attribute3]),
	?_assertNot(graph_server:add_connection({"nonexistentnodeXX1"},
											    {"node2"}) =:= ok).

delete_connection_test_() ->
	graph_server:add_node("nodeDelConn1", [attribute1, attribute2]),
	graph_server:add_node("nodeDelConn2", [attribute2, attribute3]),
	NoConnectionResponse = graph_server:delete_connection("nodeDelConn1", "nodeDelConn2"),
	graph_server:add_connection("nodeDelConn1", "nodeDelConn2"),
	[?_assertEqual({error,{noconnectionbetween,"nodeDelConn1","nodeDelConn2"}},
				  NoConnectionResponse),
	?_assertEqual(ok,
				  graph_server:delete_connection("nodeDelConn1", "nodeDelConn2"))].


add_attribute_test_() ->
	Node = "nodeattributeX",
	Node2 = "nodeattributeX2",
	graph_server:add_node(Node, [attributeX, attributeX]),
	graph_server:add_attribute(Node, newAttributeX),
	graph_server:add_node(Node2, []),
	[?_assertEqual(ok,graph_server:add_attribute(Node, newAttributeX)),
	?_assertEqual(graph_server:get_attributes(Node), {ok,[attributeX,newAttributeX]})].

add_attribute_nonexistentnode_test_() ->
	?_assertNot(graph_server:add_attribute("nonexistentnodeX", newAttributeX) =:= ok).

add_attribute_error_test_() ->
	graph_server:delete_node("nodeX1"),
	?_assertNot(graph_server:add_connection("nodeX1", attributeX) =:= ok).

get_connections_test_() ->
	graph_server:add_node("nodec1", [attribute1,attribute2]),
	graph_server:add_node("nodec2", [attribute2,attribute3,attribute4]),
	graph_server:add_node("nodec3", []),
	graph_server:add_connection("nodec1", "nodec2"),
	graph_server:add_connection("nodec1", "nodec3"),
	[?_assertEqual(graph_server:get_connections("nodec1"), {ok, ["nodec3","nodec2"]}),
	?_assertEqual(graph_server:get_connections("nodec1"), {ok,["nodec3","nodec2"]}),
		?_assert(graph_server:get_connections("nodec3") =:= {ok,["nodec1"]})].

get_connections_error_test_() -> 
		?_assert(graph_server:get_connections("nonexistentnodeX") == {ok, []}).

get_connections_noconnections_test_() -> 
	graph_server:add_node("nodenoconns1", [attribute1]),
	?_assert(graph_server:get_connections("nodenoconns1") == {ok, []}).

delete_attributes_test_() ->
	Node = "nodeint1",
	graph_server:add_node(Node, [attribute2,attribute3]),
	graph_server:add_attribute(Node, attribute1),
	{Status, AttributeList1} = graph_server:get_attributes(Node),
	graph_server:delete_attribute(Node, attribute2),
	{Status, AttributeListAfterDel} = graph_server:get_attributes(Node),
	[?_assertEqual([attribute1,attribute2,attribute3], AttributeList1),	
	?_assertEqual([attribute1,attribute3], AttributeListAfterDel) ].
					

  
clear_down_test_() ->
	Resp=file:write_file("graph_server.dat",io_lib:fwrite("~p.\n", [""]) ),
	?_assertEqual(ok, Resp).
