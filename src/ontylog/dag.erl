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
-export([init/0, delete/0, insert_links/2, all_arrows/0, 
         get_source/1, get_relation/1, get_target/1,
         get_trans_closure/3, load_dag/1, parent_relation/0]).

-include_lib("stdlib/include/qlc.hrl").
-include("dag.hrl").

init() ->
    mnesia:create_table(dag,
                        [{disc_copies, [node()]}, {type, bag}, {attributes, record_info(fields, dag)}]).

delete() ->
    mnesia:del_table_copy(dag, node()).
%%
%%
%% insert links into a graph of nodes where M is the number of distinct relations
%% to use and N is the number of nodes
%%
%%
insert_links(M, N) ->
    ListOfRelations = create_list_of_ids(M),
    Nodes = create_list_of_ids(N),
    insert_links(ListOfRelations, Nodes, N).
%%
%%
insert_links(Relations, Nodes, N) when N > 0 ->
    RelNo = length(Relations),
    NodeNo = length(Nodes),    
    insert_link(lists:nth(random:uniform(NodeNo),Nodes),
		lists:nth(random:uniform(RelNo), Relations),
		lists:nth(random:uniform(NodeNo),Nodes)),
    insert_links(Relations, Nodes, N-1);
%%
insert_links(_, _, 0) ->
    ok.
%%
%%
%%
insert_link(Source, Link, Target) ->
    mnesia:transaction(fun() ->
		  mnesia:write(#dag{source = Source,
				arrow = Link,
				target = Target})
	  end).
%%    
%%
create_list_of_ids(M) when M > 0 ->
    [couch_uuids:new() | create_list_of_ids(M-1)];
%%
create_list_of_ids(0) ->
    [].

all_arrows() ->
    F = fun() ->
                Q = qlc:q([Edge#dag.arrow || Edge <- mnesia:table(dag)], {unique, true}),
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).

get_projection(Match, Column) ->
    Q = case Column of
            arrow -> qlc:q([{Edge#dag.source, Edge#dag.target} || Edge <- mnesia:table(dag),
                                                                     Edge#dag.arrow == Match]);
            source -> qlc:q([{Edge#dag.arrow, Edge#dag.target} || Edge <- mnesia:table(dag),
                                                                     Edge#dag.source == Match]);
            target -> qlc:q([{Edge#dag.source, Edge#dag.arrow} || Edge <- mnesia:table(dag),
                                                                Edge#dag.target == Match])
        end,
    F = fun() ->
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).
                

get_relation(Relation) ->
    get_projection(Relation, arrow).get_source(Source) ->
    get_projection(Source, source).

get_target(Target) ->
    get_projection(Target, target).

get_trans_closure(Source, Arrow, []) ->
    F = fun() ->
                Q = qlc:q([Edge#dag.target || Edge <- mnesia:table(dag),
                                              Edge#dag.source == Source,
                                              Edge#dag.arrow == Arrow]),
                qlc:e(Q)
        end,
    Targets = element(2, mnesia:transaction(F)),
    case Targets of
        [] -> [];
        _ -> lists:foldl(fun(Elem, Acc) ->
                                   get_trans_closure1(Elem,Arrow,Acc)
                           end, [], Targets)
    end.
    

get_trans_closure1(Source, Arrow, VisitedNodes) ->

    AlreadySeen = lists:member(Source, VisitedNodes),

    case AlreadySeen of 
        true -> VisitedNodes;
        _ -> NewVisitedNodes = [Source | VisitedNodes],
             F = fun() ->
                         Q = qlc:q([Edge#dag.target || Edge <- mnesia:table(dag),
                                                       Edge#dag.source == Source,
                                                       Edge#dag.arrow == Arrow]),
                         qlc:e(Q)
                 end,
             Targets = element(2, mnesia:transaction(F)),
             case Targets of
                 [] -> NewVisitedNodes;
                 _ -> lists:foldl(fun(Elem, Acc) ->
                                        get_trans_closure1(Elem,Arrow,Acc)
                                end, NewVisitedNodes, Targets)
             end
    end.
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
    insert_link(SubId, parent_relation(), H),
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
    
    


                                                                 
    
