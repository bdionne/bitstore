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


