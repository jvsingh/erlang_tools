%% @author SEPStudent

-type error_reason() :: list().
-type interest_type() :: term().
-type person_type() :: {digraph:vertex()}.
-type link_graph() :: {digraph:graph()}.


%% @doc This record (digraph) had to be copied here to query the tables \n
%%    (source: digraph.erl - as of this writing, it couldn't be referred \n
%%							 directly from the main source)
-record(digraph, {vtab = notable :: ets:tab(),
		  etab = notable :: ets:tab(),
		  ntab = notable :: ets:tab(),
	          cyclic = true  :: boolean()}).
