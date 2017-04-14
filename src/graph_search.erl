%% @author jvsingh
%% @doc Graph search utilities (Erlang digraph)
%%  **Somewhat unpolished at the moment and needs improvement/review **

-module(graph_search).
-export([bfs/3]).
%% ====================================================================
%% API functions
%% ====================================================================


%% @doc function to do a breadth first traversal of the graph starting from RootNode
%%      up to the depth indicated by "Degree""
%%      currently works on bi-directional edges only 

bfs(Graph, RootNode, Degree) ->
	RootPendingList = sets:to_list(sets:from_list(
											 digraph:in_neighbours(Graph,
																   RootNode))),
	ConnList = bfs_main(Graph, RootNode, Degree, 0, [], RootPendingList ),
	UniqueSet = sets:del_element(RootNode, sets:from_list(ConnList)),
	sets:to_list(UniqueSet). %% return unique nodes only



%% ====================================================================
%% Internal functions
%% ====================================================================

%%-bfs_main(Graph, RootNode, Degree, CurrLevel, DoneList, PendingList)
%% The main loop that needs to hold a "DoneList" (confirmed connections)
%% 	and pending list. The PendingList is updated for each level
bfs_main(_Graph, _CurrNode, Degree, Degree, DoneList, _ToDoList) ->
		DoneList; %% When the Level is equal to Degree, return the DoneList
bfs_main(Graph, CurrNode, Degree, CurrLevel, DoneList, ToDoList) ->
		{NewDoneList, AllPendingList}
			= do_level(Graph, CurrNode, CurrLevel, DoneList, ToDoList, []),
		case length(AllPendingList) of
			0 -> DoneList;
			1 -> [H] = AllPendingList,
				 bfs_main(Graph, H, Degree, CurrLevel + 1, NewDoneList, []);
			_GT1 ->
				[NewCurrent|OtherPending] = AllPendingList,
				bfs_main(Graph, NewCurrent, Degree, CurrLevel + 1, NewDoneList, OtherPending)
		end.


%% do_level(Graph, RootNode, CurrLevel, DoneList, LevelPendingList, AllPendingList)
%% The purpose of do_level is to determine all the neighbours of vertices at "CurrLevel"
do_level(Graph, CurrentNode, _CurrLevel, DoneList, [], AllPendingList) ->
	CurrNeighbours = sets:to_list(sets:from_list(AllPendingList
											++ digraph:in_neighbours(Graph, CurrentNode))),
	{[CurrentNode | DoneList], AllPendingList ++ CurrNeighbours}; %% When nothing to do at this level

do_level(Graph, CurrentNode, CurrLevel, DoneList, [H|LevelPendingList], AllPendingList) ->
	case lists:member(CurrentNode, DoneList) of
		true ->
			do_level(Graph, H, CurrLevel, DoneList, LevelPendingList, AllPendingList);
		false ->
			CurrNeighbours = sets:to_list(sets:from_list(AllPendingList
											++ digraph:in_neighbours(Graph, CurrentNode))),
			do_level(Graph, H, CurrLevel, [CurrentNode|DoneList], LevelPendingList, AllPendingList ++CurrNeighbours)
	end.
