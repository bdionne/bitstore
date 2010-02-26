%%%-------------------------------------------------------------------
%%% File    : bitstore_tests.erl
%%% Author  : Robert Dionne
%%%
%%% This file is part of Bitstore.
%%%
%%% Bitstore is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% Bitstore is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with Bitstore.  If not, see <http://www.gnu.org/licenses/>.
%%%
%%% Created :  27 Jan 2010 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% bitstore, Copyright (C) 2009-2010   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(dag_tests).
-author('dionne@dionne-associates.com').
%%
%%
-include_lib("eunit/include/eunit.hrl").
%%
%%
-import(load_triple_store, [load_table/1]).
%%
%%
%%
add_simple_diamond_test() ->
    NameTable = load_table(diamond_spec()),
    Dag = dag:build_dag(hd(diamond_spec())),
    
    {ok, SubId} = dict:find(d, NameTable),
    {ok, PredId} = dict:find(p, NameTable),
    {ok, TargetId} = dict:find(a, NameTable),
    {ok, Cid} = dict:find(c, NameTable),
    {ok, Bid} = dict:find(b, NameTable),
    
    ?assert(length(dag:get_edge_targets(Dag, {SubId,PredId})) =:= 2),
    Definition = dag:get_edges(Dag,SubId),
    ?assert(length(Definition) =:= 1),
    [{Key, [Val1, Val2]}] = Definition,
    ?assert(Key =:= PredId),
    ?assert(Val1 =:= Bid),
    ?assert(Val2 =:= Cid),    
    ?assert(dag:path_exists(Dag, {SubId,PredId,TargetId})),
    ?assert(not dag:path_exists(Dag, {Cid,PredId,Bid})).

diamond_spec() ->
    [diamond, [{d, p, c},
           {d, p, b},
           {c, p, a},
           {b, p, a}]].

tree_test() ->
    Dag = build_tree(1,15),
    ?assert(dict:size(Dag) =:= 32767),
    dag:persist_dag(Dag,tree).

build_tree(1,N) ->
    Dag = dag:build_dag(tree),
    {Pid, NewDag} = dag:find_or_create_pid(list_to_binary(integer_to_list(1)),Dag),
    build_tree1(2,N,[Pid],NewDag).

build_tree1(I,N,PidList,Dag) ->
    NewInts = lists:seq(round(math:pow(2,(I-1))),round(math:pow(2,I)) - 1),
    [Dag1, NewPidList] = 
        lists:foldl(
          fun(Id,Acc) ->
                  {Pid, NewDag} = 
                      dag:find_or_create_pid(list_to_binary(integer_to_list(Id)),
                                             hd(Acc)),
                  ParPid = lists:nth(Id div I,PidList),
                  [dag:add_edge(NewDag,
                                {dag:id(Pid),<<"666">>,dag:id(ParPid)}),
                   [Pid | hd(tl(Acc))]]
          end,[Dag,[]],NewInts),
    case I == N of
        true ->
            Dag1;
        _ ->
            build_tree1(I+1,N,NewPidList,Dag1)
    end.
                      
    
    
    
    
    
    
    


