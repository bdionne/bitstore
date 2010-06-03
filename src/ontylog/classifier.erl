%%%-------------------------------------------------------------------
%%% File    : classifier.erl
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
%%% Created :  19 Jan 2010 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% bitstore, Copyright (C) 2009-2010   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(classifier).
-author('dionne@dionne-associates.com').

-include("bitstore.hrl").

-export([classify/3,
         subsumes_p/2]).
%%
-import(bitcask, [get/2,put/3,fold/3]).
-import(digraph, [new/1,add_vertex/1,add_vertex/3,add_edge/4, vertex/2]).
-import(lists, [map/2]).
%%
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%%
%%
%% 0. topologically sort concepts with respect to the "isa" relation
%% 1. compute the LUBs for the concept
%% 2. using the LUBs as roots, compute the GLBs
%% 
%% If 1. and 2. result in a single concept and it's the same in both cases, then we
%% have an Eq.
%%
%% 3. remove any subsumptions between any of the GLBs and LUBs
%% 4. add any new subsumptions given in the GLBs and LUBs not already accounted for
%%    by definitions
%%
%% Normally this requires marking algorithms to make it efficient, detect diamonds, etc..
%% it's not clear how to pull this off without writing some temporary bits in the cask. Perhaps
%% the digraph and digraph_utils modules will be sufficient, as they support labels on vertices.
%% They can be used to store colors, prim/def bits, visited, and classified bits for recursive 
%% calls
%%
%%
%%
%% classification in traditional DLs is always with respect to the "isa" relation but
%% one can imagine other interencing algorithms over different relations
%%
classify(DagCask,Arrow,ClassifyFun) ->

    %% create new digraph and add vertex for each node in the cask, using the key for the label
    %% add single edge for each link
    Dag = new([acyclic]),

    Vids = ets:new(vertex_ids,[set]),

    %% nothing can happen without ***Thing***
    Root = add_labeled_vertex(Dag,<<"0">>),    
    
    %% first get all vertices created so that forward refences can be resolved 
    fold(DagCask,
         fun(K,_Concept,Tab) ->
                 ?LOG(?DEBUG,"creating vertex for ~p ~n",[K]),
                 ets:insert(Tab,{K, add_labeled_vertex(Dag,K)}),
                 Tab
         end,Vids),

    %% now add all the edges. Note that we could get all the relations from each node
    %% in this single pass, which is more efficient, but correctness comes first so 
    %% we'll keep it dirt simple
    fold(DagCask,
         fun(K,Concept,Tab) ->
                 case proplists:lookup(Arrow,element(1, binary_to_term(Concept))) of
                     none ->
                         %% node is a root wrt Arrow relation, add ***Thing*** as parent
                         add_edge(Dag,find_vertex(K,Tab),Root,Arrow);
                     {Arrow, Targets} ->
                         map(fun(Target) ->
                                     add_edge(Dag,find_vertex(K,Tab),find_vertex(Target,Tab),
                                              Arrow)
                             end,Targets)
                 end,
                 Tab
         end,Vids),

    %% Visit concepts in topological order and classify each
    Inferences = map(fun(Concept) ->
                             ClassifyFun(Dag, Concept)
                     end, 
                     lists:reverse(digraph_utils:topsort(Dag))),
    map(fun(Inf) ->
                ?LOG(?DEBUG,"The inference is ~p ~n",[element(2,Inf)])
        end,Inferences),
    %%
    %% store the new inferecences in the cask
    map(fun(NewFacts) ->
                store_new_facts(DagCask,NewFacts)
        end,Inferences),
    
    ok.

classify_con(Dag, Concept) ->
    V = vertex(Dag, Concept),
    ?LOG(?DEBUG,"classifying ~p ~n",[element(2,V)]),
    V.
    
    
                 
%%
%%
subsumes_p(_Node1,_Node2) ->
    true.
%%
%%
store_new_facts(_DagCask, _NewFacts) ->
    ok.
%%
%%
add_labeled_vertex(Dag,Label) ->
    V = add_vertex(Dag),
    add_vertex(Dag,V,Label).
%%
%%
find_vertex(Key,Tab) ->
    case ets:lookup(Tab,Key) of
        [] ->
             [];
        [{Key, V}] -> V
    end.
%% 
%% EUnit tests
%% 
-ifdef(TEST).
%%
topo_sort_test() ->
    %% simple diamon with single root node
    Dag = dag:create_or_open_dag("onty1",true),
    dag:add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    dag:add_edge({<<"004">>,<<"002">>,<<"003">>},Dag),
    dag:add_edge({<<"005">>,<<"002">>,<<"004">>},Dag),
    dag:add_edge({<<"005">>,<<"002">>,<<"001">>},Dag),
    classify(Dag,<<"002">>,fun classify_con/2).
                          
    
-endif.
