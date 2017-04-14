%% @author jvsingh
%% @doc @todo Add description to graph_search_tests.

-include_lib("eunit/include/eunit.hrl").

-module(graph_search_tests).

get_graph() ->
	G = digraph:new(),
	digraph:add_vertex(G, "vertex1"),
	digraph:add_vertex(G, "vertex2"),
	digraph:add_vertex(G, "vertex3"),
	digraph:add_vertex(G, "vertex4"),
	digraph:add_vertex(G, "vertex5"),
	digraph:add_vertex(G, "vertex5"),
	digraph:add_vertex(G, "vertex6"),

	digraph:add_edge(G, "vertex1", "vertex2"),
	digraph:add_edge(G, "vertex2", "vertex1"),
	
	
	digraph:add_edge(G,"vertex2", "vertex3"),
	digraph:add_edge(G,"vertex3", "vertex2"),
	
	digraph:add_edge(G,"vertex2", "vertex4"),
	digraph:add_edge(G,"vertex4", "vertex2"),
	
	digraph:add_edge(G,"vertex2", "vertex5"),
	digraph:add_edge(G,"vertex5", "vertex2"),
	G.


degree_one_test_() ->
	G = get_graph(),
	Response = graph_search:bfs(G, "vertex1", 1),
	?_assertEqual(["vertex2"], Response).

degree_two_test_() ->
	G = get_graph(),
	Response = graph_search:bfs(G, "vertex1", 2),
	?_assertEqual(["vertex2","vertex3","vertex4","vertex5"], 
				   lists:sort( Response )).

