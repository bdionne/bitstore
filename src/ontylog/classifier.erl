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

-export([classify/3]).
%%
-import(bitcask, [get/2,put/3,list_keys/1]).
-import(digraph, [new/1,add_vertex/1,add_vertex/3,
                  get_path/3,
                  add_edge/4, edges/2, vertex/2]).
-import(lists, [map/2, all/2, any/2, reverse/1]).
%%
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%%
%%
%% This classifier computes the classification order with respect to a given relation. Traditionally
%% this relation is the "isa" relation but one could imagine using others such as "part-of". However
%% since most semantics are given in terms of set theoretic models where "isa" corresponds to subset
%% inclusion, it's hard to see these other cases. Nevertheless it's easy enough to make it this general
%% so why not.
%%
%% 0. topologically sort concepts with respect to their definitions
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
%% classification in traditional DLs is always with respect to the "isa" relation but
%% one can imagine other inferencing algorithms over different relations
%%
classify(DagCask,Arrow,ClassifyFun) ->

    %% create new digraph and add vertex for each node in the cask, 
    %% using the key for the label 
    Dag = new([acyclic]),

    %% create hash to lookup nodes by id
    Vids = ets:new(vertex_ids,[set]),
    %%
    %% store reference to persistent Dag and id hash
    put(<<"cask">>,DagCask),
    put(<<"id_tab">>,Vids),
    put(<<"children">>,Arrow),

    Keys = list_keys(DagCask),
    %%
    %% first get all vertices created so that forward refences can be resolved 
    map(fun(K) ->
                ?LOG(?DEBUG,"creating vertex for ~p ~n",[K]),
                 ets:insert(Vids,{K, add_labeled_vertex(Dag,{K,not_classified})})
        end,
        Keys),

    %% now add all the edges. For each node get all the edges
    %% from it, both "isa" edges and roles.
    map(fun(K) ->
                case dag:get_targets(K,DagCask) of
                    [] ->
                        ok;
                    Targets ->
                        map(fun({Edge,Values}) ->
                                    map(fun(Value) ->
                                                add_edge(Dag,
                                                         find_vertex(K,Vids),
                                                         find_vertex(Value,Vids),
                                                         Edge)
                                        end,Values)
                            end,Targets)
                end
        end, Keys),

    SortedConcepts = lists:reverse(digraph_utils:topsort(Dag)),

    %% nothing can happen without ***Thing***    
    ets:insert(Vids,{thing(),add_labeled_vertex(Dag,{thing(),classified})}),

    %% Visit concepts in topological order and classify each
    %% As each concept is classified new edges may be added to
    %% vertices in the graph. The inferences returned will be triples
    %% suitable for insertion in the cask, .ie. of the form:
    %% {subj,pred,obj}
    %%
    Inferences = map(fun(Concept) ->
                             ClassifyFun(Vids,Dag,Concept)
                     end,
                     SortedConcepts),
    ?LOG(?DEBUG,"There are ~p inferences.~n",[Inferences]),
    map(fun(Inf) ->
                case Inf of
                    [] ->
                        ?LOG(?DEBUG,"The inference is empty ~n",[]);
                    _ -> ?LOG(?DEBUG,"The inference is ~p ~n",[element(2,Inf)])
                end
        end,Inferences),
    %%
    %% store the new inferecences in the cask
    map(fun(NewFacts) ->
                store_new_facts(DagCask,NewFacts)
        end,Inferences),    
    ok.

parent_count(Dag,Concept) ->
    Arrow = get(<<"children">>),
    lists:foldl(fun(Edge,Acc) ->
                        {_,_,_,Label} = digraph:edge(Dag,Edge),
                        case Label == Arrow of
                            true ->
                                Acc + 1;
                            false ->
                                Acc
                        end
                end,0,edges(Dag,Concept)).

children(Dag,Concept) ->
    Arrow = get(<<"children">>),
    lists:foldl(fun(Vertex, Acc) ->
                map(fun(Edge) ->
                            {_,_,V2,Label} = digraph:edge(Dag,Edge),
                        case Label == Arrow of
                            true ->
                                case V2 == Concept of
                                    true ->
                                        [Vertex | Acc];
                                    false ->
                                        Acc
                                end;
                            false ->
                                Acc
                        end end, edges(Dag,Vertex))
                end,[],digraph:in_neighbours(Dag,Concept)).
    
    

classify_con(LookUpTab, Dag, Concept) ->
    print_concept(Dag,Concept),
    Thing = find_vertex(thing(),LookUpTab),
    %%print_concept(Dag,Thing),
    %% ***Thing*** subsumes all concepts so we start there
    case parent_count(Dag,Concept) of
        0 ->
            %% Concept has no parents to add ***Thing***
            add_edge(Dag,Concept,Thing,get(<<"children">>));
        _ ->
            ok
    end,
    %% first compute the least upper bounds, those concepts
    %% that subsume the given concept and are subsumed by any other
    %% concept that subsumes it.
    Lubs = find_lubs(Dag,Thing,Concept,[]),
    ?LOG(?DEBUG,"The Lubs are ~p ~n",[Lubs]),
    %%
    %% treating each Lub as a root now find the Glbs
    Glbs = sl_flatten(map(fun(Lub) ->
                find_glbs(Dag,Lub,Concept,[])
        end,Lubs)),
    ?LOG(?DEBUG,"The Glbs are ~p ~n",[Glbs]),
    %%
    %% 
    create_inferred_facts(Dag,Lubs,Glbs,Concept). 
    
%%
%%
find_lubs(Dag,PossSubsumer,Concept,Lubs) ->
    ?LOG(?DEBUG,"calling find_lubs with ~p ~p ~p ~n",[PossSubsumer,Concept,Lubs]),
    %%print_concept(Dag,PossSubsumer),
    NewLubs = case PossSubsumer == Concept of
                  true ->
                      ?LOG(?DEBUG,"ok, these guyes are eq ~p ~p ~n",[PossSubsumer,Concept]),
                      Lubs;
                  _ ->
                      case is_greater(Dag,PossSubsumer,Concept) of
                          true ->
                              add_lub(Dag,PossSubsumer,Lubs);
                          _ ->
                              Lubs
                      end
              end,
    case NewLubs /= Lubs of
        true ->
            Children = children(Dag,PossSubsumer),
            case Children of
                [] ->
                    NewLubs;
                _ ->
                    sl_flatten(map(fun(Child) ->
                                find_lubs(Dag,Child,Concept,NewLubs)
                        end,Children))
            end;
        false -> NewLubs
    end.
%%
%%
find_glbs(Dag,PossSubsumee,Concept,Glbs) ->
    ?LOG(?DEBUG,"calling find_glbs with ~p ~p ~p ~n",[PossSubsumee,Concept,Glbs]),
    %%print_concept(Dag,PossSubsumee),
    NewGlbs = case PossSubsumee == Concept of
                  true ->
                      ?LOG(?DEBUG,"ok, these guyes are eq ~p ~p ~n",[PossSubsumee,Concept]),
                      Glbs;
                  _ ->
                      case is_greater(Dag,Concept,PossSubsumee) of
                          true ->
                              add_glb(Dag,PossSubsumee,Glbs);
                          _ -> 
                              Glbs
                      end
              end,
    case NewGlbs /= Glbs of
        true ->
            Children = children(Dag,PossSubsumee),
            case Children of
                [] ->
                    NewGlbs;
                _ -> 
                     sl_flatten(map(fun(Child) ->
                                find_glbs(Dag,Child,Concept,NewGlbs)
                        end,Children))
            end;
        false -> 
            NewGlbs
    end.    
%%
%%
add_lub(Dag,Lub,Lubs) ->
    add_con_if_satisfies(
      Dag,
      Lub,
      Lubs,
      fun(Elem) ->
              subsumes_p(Dag,Elem,Lub)
      end).
%%
%%
add_glb(Dag,Glb,Glbs) ->
    add_con_if_satisfies(
      Dag,
      Glb,
      Glbs,
      fun(Elem) ->
              subsumes_p(Dag,Glb,Elem)
      end).
%%
%%
add_con_if_satisfies(Dag,Con,Cons,Fun) ->
    ?LOG(?DEBUG,"checking adding con to list ~p ~p ~n",[Con,Cons]),
    %%print_concept(Dag,Con),
    ConsToRemove = 
        lists:foldl(fun(Elem, Acc) ->
                            case Fun(Elem) of
                                true -> lists:append(Acc, [Elem]); 
                                _ -> Acc
                            end
                    end,[],Cons),
    NewCons = lists:subtract(Cons,ConsToRemove),
    ?LOG(?DEBUG,"ok, adding concept ~p ~n",[Con]),            
    lists:append(NewCons,[Con]).        
%%
%%
create_inferred_facts(_Dag,_Lubs,_Glbs,_Concept) ->
    [].                 
%%
%% compare the definitions of two concepts
%% this is where all the logical work in classification is
is_greater(Dag,Subsumer,Subsumee) ->
    SupKey = extract_key(Dag,Subsumer),
    SubKey = extract_key(Dag,Subsumee),
    DagCask = get(<<"cask">>),
    SupDef = dag:get_targets(SupKey,DagCask),
    SubDef = dag:get_targets(SubKey,DagCask),
    ?LOG(?DEBUG,"the definition of ~p is ~p ~n",[SupKey,SupDef]), 
    ?LOG(?DEBUG,"the definition of  ~p is  ~p ~n",[SubKey,SubDef]),
    %% all works even if SupDef is empty, as it should given all is vacuously true
    %% it is a thing of beauty to be able to state something so succintly
    all(fun(SupRel) ->
                any(fun(SubRel) ->
                            rel_subsumes_p(Dag,SupRel,SubRel)
                    end,SubDef)
        end,SupDef).
    
   
%%
%% subsumes_p merely tests if the two concepts are already in the subsumes
%% relation, either directly or transitively 
subsumes_p(Dag,Subsumer,Subsumee) ->
    case get_path(Dag,Subsumee,Subsumer) of
        false -> 
            false;
        _ ->
            ?LOG(?DEBUG,"subsumes_p found a path ~p ~p ~n",[Subsumee,Subsumer]),
            true
    end.
%%
%%
rel_subsumes_p(Dag,SupRel,SubRel) ->
    ?LOG(?DEBUG,"checking role vals ~p ~p ~n",[SupRel,SubRel]),
    IdTab = get(<<"id_tab">>),
    case element(1,SupRel) == element(1,SubRel) of
        false ->
            false;
        true -> 
            all(fun(Sup) ->
                        any(fun(Sub) ->
                                    subsumes_p(Dag,find_vertex(Sup,IdTab),
                                               find_vertex(Sub,IdTab))
                            end,element(2,SubRel))
                end,element(2,SupRel))
    end.
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
extract_key(Dag,Concept) ->
    element(1,element(2,vertex(Dag,Concept))).
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
    ?LOG(?DEBUG,"concept ~p ~n",[element(2,vertex(Dag, Concept))]).
%%
%%
sl_flatten([]) -> [];
sl_flatten([[]]) -> [];
sl_flatten([H | T]) ->
    [hd(H) | sl_flatten(T)].


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
    classify(Dag,<<"002">>,fun classify_con/3),
    bitcask:close(Dag).

simple_lub_test() ->
    Dag = dag:create_or_open_dag("onty2",true),
    dag:add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    dag:add_edge({<<"005">>,<<"002">>,<<"004">>},Dag),
    dag:add_edge({<<"005">>,<<"0013">>,<<"003">>},Dag),
    dag:add_edge({<<"006">>,<<"002">>,<<"004">>},Dag),
    dag:add_edge({<<"006">>,<<"0013">>,<<"001">>},Dag),
    classify(Dag,<<"002">>,fun classify_con/3),
    bitcask:close(Dag).
    
                          
    
-endif.
