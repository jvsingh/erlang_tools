# erlang_tools

Contents:
---------
1) A graph server based on the Erlang digraph
   It includes Nodes that are connected to each other using labeled bi-directional edges
   Each Node can have attributes attached using labeled directed edges 
   The graph server includes a handy API to create and update contents of the graph
   
2) graph_search
   A handy set of utilities that can be used to search the Erlang digraph
   (e.g. search nodes up to a certain Depth within the neighbourhood of the digraph)
   I created this to address requirements that could not be met by functions available
   in either digraph or digraph_utils within the main Erlang distribution
   
   
Tests
------
A set of eunit tests are available in the "test" folder
These can be run using "rebar3 eunit" or another test harness 
   
