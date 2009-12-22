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
-export([load_dag/1]).

-import(triple_store, [insert_tuple/3]).


getTriple(Triple) ->
    case Triple of
        [{atom,_,Subject},
         {atom,_,Predicate},
         {atom,_,Object}] ->
            [Subject,Predicate,Object];
        _ -> []
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

store_triple(TripNodeIds) ->
    case TripNodeIds of
        [{_, SubId},
         {_, PredId},
         {_, ObjId}] ->
            insert_tuple(SubId, PredId, ObjId);
        _ -> []
    end.

build_dag(File, NodeDict) ->
    case io:get_line(File,'') of
        eof -> NodeDict;
        LS -> 
            Triple = getTriple(element(2,erl_scan:string(LS))),
            {TripleNodeIdPairs, NewNodeDict} = assign_ids(Triple, NodeDict, []),
            store_triple(TripleNodeIdPairs),
            build_dag(File, NewNodeDict)
    end.

load_dag(Dag) ->
    io:format("Loading Dag: ",[]),
    NodeDict = build_dag(element(2,file:open(Dag, [read])), dict:new()),
    io:format("Dag Built ~n",[]),
    NodeDict.
    
    


                                                                 
    
