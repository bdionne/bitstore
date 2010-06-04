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
-import(digraph, [new/1,add_vertex/1,add_vertex/3,
                  in_neighbors/2,
                  add_edge/4, out_degree/2, vertex/2]).
-import(lists, [map/2, reverse/1]).
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

    %% create new digraph and add vertex for each node in the cask, 
    %% using the key for the label 
    Dag = new([acyclic]),

    Vids = ets:new(vertex_ids,[set]),    
    
    %% first get all vertices created so that forward refences can be resolved 
    fold(DagCask,
         fun(K,_Concept,Tab) ->
                 ?LOG(?DEBUG,"creating vertex for ~p ~n",[K]),
                 ets:insert(Tab,{K, add_labeled_vertex(Dag,{K,not_classified})}),
                 Tab
         end,Vids),

    %% now add all the edges. Note that we could get all the relations from each node
    %% in this single pass, which is more efficient, but correctness comes first so 
    %% we'll keep it dirt simple
    fold(DagCask,
         fun(K,Concept,Tab) ->
                 case proplists:lookup(Arrow,element(1, binary_to_term(Concept))) of
                     none ->
                         %% concept is a root, nothing to add
                         ok;
                     {Arrow, Targets} ->
                         map(fun(Target) ->
                                     add_edge(Dag,find_vertex(K,Tab),find_vertex(Target,Tab),
                                              Arrow)
                             end,Targets)
                 end,
                 Tab
         end,Vids),

    SortedConcepts = lists:reverse(digraph_utils:topsort(Dag)),

    %% nothing can happen without ***Thing***
    add_labeled_vertex(Dag,{thing(),classified}),

    %% Visit concepts in topological order and classify each
    %% As each concept is classified new edges may be added to
    %% vertices in the graph. The inferences returned will be triples
    %% suitable for insertion in the cask, .ie. of the form:
    %% {subj,pred,obj}
    %%
    Inferences = map(fun(Concept) ->
                             ClassifyFun(Dag,Concept,Arrow)
                     end,
                     SortedConcepts),
    map(fun(Inf) ->
                ?LOG(?DEBUG,"The inference is ~p ~n",[element(2,Inf)])
        end,Inferences),
    %%
    %% store the new inferecences in the cask
    map(fun(NewFacts) ->
                store_new_facts(DagCask,NewFacts)
        end,Inferences),
    
    ok.

classify_con(Dag, Concept, Arrow) ->
    print_concept(Dag,Concept),
    Thing = find_vertex(thing(),Dag),
    %% Thing subsumes all concepts so we start there
    case out_degree(Dag,Concept) of
        0 -> 
            add_edge(Dag,Concept,Thing,Arrow);
        _ ->
            ok
    end,
    %% first compute the least upper bounds, those concepts
    %% that subsume the given concept and are subsumed by any other
    %% concept that subsumes it.
    Lubs = find_lubs(Dag,Thing,Concept,[]),
    %%
    %% treating each Lub as a root now find the Glbs
    Glbs = map(fun(Lub) ->
                find_glbs(Dag,Lub,Concept,[])
        end,Lubs),
    %%
    %% 
    create_inferred_facts(Dag,Lubs,Glbs,Concept).
    
    
%%
%%
find_lubs(Dag,PossSubsumer,Concept,Lubs) ->
    NewLubs = case PossSubsumer == Concept of
                  true ->
                      Lubs;
                  _ ->
                      case is_greater(Dag,PossSubsumer,Concept) of
                          true ->
                              add_if_least(Dag,PossSubsumer,Lubs);
                          _ ->
                              Lubs
                      end
              end,
    Children = in_neighbors(Dag,PossSubsumer),
    case Children of
        [] ->
            Lubs;
        _ ->
            map(fun(Child) ->
                        find_lubs(Dag,Child,Concept,NewLubs)
                end,Children)
    end.
%%
%%
find_glbs(_Dag,_PossSubsumee,_Concept,_Glbs) ->
    %% this is the part I hate the most
    ok.
    
%%
%%
add_if_least(_Dag,_Lub,_Lubs) ->
    ok.
%%
%%
create_inferred_facts(_Dag,_Lubs,_Glbs,_Concept) ->
    ok.                 
%%
%% compare the definitions of two concepts
%% this is where all the logical work in classification is
is_greater(_Dag,_Subsumer,_Subsumee) ->
    true.
    
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
%%
thing() ->
    <<"0">>.
%%
%%
print_concept(Dag, Concept) ->
    ?LOG(?DEBUG,"classifying ~p ~n",[element(2,vertex(Dag, Concept))]).

%% 
%% EUnit tests
%% 
-ifdef(TEST).
%%
topo_sort_test() ->
    %% simple diamond with single root node
    Dag = dag:create_or_open_dag("onty1",true),
    dag:add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    dag:add_edge({<<"004">>,<<"002">>,<<"003">>},Dag),
    dag:add_edge({<<"005">>,<<"002">>,<<"004">>},Dag),
    dag:add_edge({<<"005">>,<<"002">>,<<"001">>},Dag),
    classify(Dag,<<"002">>,fun classify_con/3).
                          
    
-endif.
