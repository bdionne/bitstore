%%%-------------------------------------------------------------------
%%% File    : dag.erl
%%% Author  : Robert Dionne
%%% Description : 
%%%
%%% Created :  2 Sep 2009 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% bitstore, Copyright (C) 2009   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(dag).
-author('dionne@dionne-associates.com').
%%
%%
%%
-export([insert_arrows/2, 
	 load_dag/1, parent_relation/0]).

-import(triple_store, [insert_tuple/3]).

%%
%%
%% insert links into a graph of nodes where M is the number of distinct relations
%% to use and N is the number of nodes
%%
%%
insert_arrows(M, N) ->
    ListOfRelations = create_list_of_ids(M),
    Nodes = create_list_of_ids(N),
    insert_arrows(ListOfRelations, Nodes, N).
%%
%%
insert_arrows(Relations, Nodes, N) when N > 0 ->
    RelNo = length(Relations),
    NodeNo = length(Nodes),    
    insert_tuple(lists:nth(random:uniform(NodeNo),Nodes),
		lists:nth(random:uniform(RelNo), Relations),
		lists:nth(random:uniform(NodeNo),Nodes)),
    insert_arrows(Relations, Nodes, N-1);
%%
insert_arrows(_, _, 0) ->
    ok.
%%
create_list_of_ids(M) when M > 0 ->
    [couch_uuids:new() | create_list_of_ids(M-1)];
%%
create_list_of_ids(0) ->
    [].

%% some helper utilities for building various graphs
parent_relation() ->
    <<"0000000001">>.

getAtoms([]) -> [];
getAtoms([H|T]) -> case H of
                        {atom,_, A} -> [A | getAtoms(T)];
                        _ -> getAtoms(T)
                    end.

assign_ids([H | T], NodeDict, NodePairs) ->
    case dict:find(H, NodeDict) of
        {ok, Value} ->
            assign_ids(T, NodeDict, [{H, Value} | NodePairs]);
        error -> NodeId = couch_uuids:new(),
                 assign_ids(T, dict:store(H, NodeId, NodeDict),
                            [{H, NodeId} | NodePairs])
    end;
assign_ids([], NodeDict, NodePairs) ->
    {lists:reverse(NodePairs), NodeDict}.

build_arcs(NodeList) ->
    {_, SubId} = hd(NodeList),
    connect_node(SubId, lists:map(fun(Pair) -> element(2, Pair) end,
                                  tl(NodeList))).

connect_node(SubId, [H | T]) ->
    insert_tuple(SubId, parent_relation(), H),
    connect_node(SubId, T);
connect_node(_, []) ->
    ok.

build_dag(File, NodeDict) ->
    case io:get_line(File,'') of
        eof -> NodeDict;
        LS -> 
            Atoms = getAtoms(element(2,erl_scan:string(LS))),
            {Nodes, NewNodeDict} = assign_ids(Atoms, NodeDict, []),
            build_arcs(Nodes),
            build_dag(File, NewNodeDict)
    end.

load_dag(Dag) ->
    io:format("Loading Dag: ",[]),
    NodeDict = build_dag(element(2,file:open(Dag, [read])), dict:new()),
    io:format("Dag Built ~n",[]),
    NodeDict.
    
    


                                                                 
    
