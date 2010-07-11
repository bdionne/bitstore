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
                  get_path/3, edges/2,
                  add_edge/4, out_neighbours/2, vertex/2]).
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
%% 5. write new inferences back to the persistent store
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
                %%?LOG(?DEBUG,"creating vertex for ~p ~n",[K]),
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
                                                %%?LOG(?DEBUG,"adding edge ~p ~p ~p ~n",
                                                  %%   [K,Edge,Value]),
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
    %% and of course ***Thing*** subsumes everything
    ets:insert(Vids,{thing(),add_labeled_vertex(Dag,{thing(),classified})}),

    %% Visit concepts in topological order and classify each
    %% As each concept is classified new edges may be added to
    %% vertices in the graph. Concepts will never be compared to
    %% concepts that are not yet classified. If a concept is changed 
    %% from it's original definition due to clasification it is marked
    %% classified_modified so that the new result can be written out 
    %% to the cask
    %%
    map(fun(Concept) ->
                ClassifyFun(Vids,Dag,Concept)
        end, SortedConcepts),
    %%
    %% store classified concepts in the cask if needed
    map(fun(Concept) ->
                case is_classified_modified(Dag,Concept) of
                    true ->
                        store_new_facts(Dag,Concept);
                    _ ->
                        ok
                end
        end,SortedConcepts),    
    ok.

parent_count(Dag,Concept) ->
    Arrow = get(<<"children">>),
    %% ?LOG(?DEBUG,"This concept ~p has ~p edges ~n",
    %%      [label(Dag,Concept),length(edges(Dag,Concept))]),
    lists:foldl(fun(Edge,Acc) ->
                        {_,_,V2,Label} = digraph:edge(Dag,Edge),
                        case Label == Arrow of
                            true ->
                                case V2 == Concept of
                                    true ->
                                        Acc;
                                    false ->
                                        Acc + 1
                                end;
                            false ->
                                Acc
                        end
                end,0,edges(Dag,Concept)).

is_parent(Dag,Concept,Parent) ->
    Arrow = get(<<"children">>),
    any(fun(Edge) ->
                {_,_,V2,Label} = digraph:edge(Dag,Edge),
                case Label == Arrow of
                    true ->
                        case V2 == Concept of
                            true ->
                                false;
                            false ->
                                case V2 == Parent of
                                    true ->
                                        true;
                                    false ->
                                        is_parent(Dag,V2,Parent)
                                end
                        end;
                    false ->
                        false
                end
        end,edges(Dag,Concept)).

children(Dag,Concept) ->
    Arrow = get(<<"children">>),
    lists:foldl(fun(Vertex, Acc) ->
                        %% ?LOG(?DEBUG,"checking vertex ~p ~n",
                        %%      [label(Dag,Vertex)]),
                        case any(fun(Edge) ->
                                         {_,_,V2,Label} = digraph:edge(Dag,Edge),
                                         %% ?LOG(?DEBUG,"The label is ~p and the Node is ~p ~n",
                                         %%      [Label, label(Dag,V2)]),
                                         case Label == Arrow of
                                             true ->
                                                 case V2 == Concept of
                                                     true ->
                                                         true;
                                                     %% ?LOG(?DEBUG,"true true ~p ~p ~p ~p ~n",
                                                     %%      [Label,Arrow,V2,Concept]),
                                                     %% Acc ++ [Vertex];
                                                     false ->
                                                         false
                                                 end;
                                             false ->
                                                 false
                                         end end, digraph:edges(Dag,Vertex)) of
                            true ->
                                Acc ++ [Vertex];
                            false ->
                                Acc
                        end
                end,[],digraph:in_neighbours(Dag,Concept)).    

classify_con(LookUpTab, Dag, Concept) ->
    print_concept(Dag,Concept),
    Thing = find_vertex(thing(),LookUpTab),
    %% ***Thing*** subsumes all concepts so we start there
    case parent_count(Dag,Concept) of
        0 ->
            %% Concept has no parents to add ***Thing***
            ?LOG(?DEBUG,"adding Thing as parent ~n",[]),
            add_edge(Dag,Concept,Thing,get(<<"children">>));
        Count ->
            ?LOG(?DEBUG,"This concept ~p has ~p parents ~n",
                 [label(Dag,Concept),Count]),
            ok
    end,
    %% first compute the least upper bounds, those concepts
    %% that subsume the given concept and are subsumed by any other
    %% concept that subsumes it.
    Lubs = find_lubs(Dag,Thing,Concept,[]),
    ?LOG(?DEBUG,"The Lubs are ~p ~n",[labels(Dag,Lubs)]),
    FilteredLubs = lists:foldl(fun(E,Acc) ->
                                      add_lub(Dag,E,Acc)
                              end,[],Lubs),
    ?LOG(?DEBUG,"The Filtered Lubs are ~p ~n",[labels(Dag,FilteredLubs)]),
                                      
    %%
    %% treating each Lub as a root now find the Glbs
    Glbs = sl_flatten(map(fun(Lub) ->
                find_glbs(Dag,Lub,Concept,[])
        end,FilteredLubs)),
    ?LOG(?DEBUG,"The Glbs are ~p ~n",[labels(Dag,Glbs)]),

    FilteredGlbs = lists:foldl(fun(G,Acc) ->
                                       add_glb(Dag,G,Acc)
                               end,[],Glbs),
    %%
    %% 
    create_inferred_facts(Dag,FilteredLubs,FilteredGlbs,Concept). 
    
%%
%%
find_lubs(Dag,PossSubsumer,Concept,Lubs) ->
    %%?LOG(?DEBUG,"calling find_lubs with ~p ~p ~p ~n",[label(Dag,PossSubsumer),label(Dag,Concept),labels(Dag,Lubs)]),
    NewLubs = case PossSubsumer == Concept of
                  true ->
                      ?LOG(?DEBUG,"ok, these guyes are eq ~p ~p ~n",[label(Dag,PossSubsumer),label(Dag,Concept)]),
                      Lubs;
                  _ ->
                      case is_parent(Dag,Concept,PossSubsumer) orelse
                          is_greater(Dag,PossSubsumer,Concept) of
                          true ->
                              add_lub(Dag,PossSubsumer,Lubs);
                          false ->
                              Lubs
                      end
              end,
    case NewLubs /= Lubs of
        true ->
            Children = children(Dag,PossSubsumer),
            %% ?LOG(?DEBUG,"the number of children is ~p ~p ~n",
            %%      [length(Children),Children]),
            %%?LOG(?DEBUG,"the children of ~p are ~p ~n",[label(Dag,PossSubsumer),labels(Dag,Children)]),
            case Children of
                [] ->
                    NewLubs;
                _ ->
                    sl_flatten(map(fun(Child) ->
                                           case is_classified(Dag,Child) of
                                               true ->
                                                   find_lubs(Dag,Child,Concept,NewLubs);
                                               false -> NewLubs
                                           end end,Children))
            end;
        false -> NewLubs
    end.
%%
%%
find_glbs(Dag,PossSubsumee,Concept,Glbs) ->
    %%?LOG(?DEBUG,"calling find_glbs with ~p ~p ~p ~n",[label(Dag,PossSubsumee),label(Dag,Concept),labels(Dag,Glbs)]),
    NewGlbs = case PossSubsumee == Concept of
                  true ->
                      ?LOG(?DEBUG,"ok, these guyes are eq ~p ~p ~n",[label(Dag,PossSubsumee),label(Dag,Concept)]),
                      Glbs;
                  _ ->
                      case is_parent(Dag,PossSubsumee,Concept) orelse
                          is_greater(Dag,Concept,PossSubsumee) of
                          true ->
                              add_glb(Dag,PossSubsumee,Glbs);
                          _ -> 
                              Glbs
                      end
              end,
    case NewGlbs /= Glbs of
        false ->
            Children = children(Dag,PossSubsumee),
            %%?LOG(?DEBUG,"the children of ~p are ~p ~n",
                 %%[label(Dag,PossSubsumee),labels(Dag,Children)]),
            case Children of
                [] ->
                    NewGlbs;
                _ -> 
                     sl_flatten(map(fun(Child) ->
                                            case is_classified(Dag,Child) of
                                                true ->
                                                    find_glbs(Dag,Child,Concept,NewGlbs);
                                                false -> NewGlbs
                                            end end,Children))
            end;
        true -> 
            NewGlbs
    end.    
%%
%%
add_lub(Dag,Lub,Lubs) ->
    case any(fun(L) -> subsumes_p(Dag,Lub,L) end,Lubs) of
        true ->
            case length(Lubs) of
                0 ->
                    [Lub];
                _ ->
                    Lubs
            end;
        false ->
            add_con_if_satisfies(
              Dag,
              Lub,
              Lubs,
              fun(Elem) ->
                      subsumes_p(Dag,Elem,Lub)
              end)
    end.
%%
%%
add_glb(Dag,Glb,Glbs) ->
    case any(fun(G) -> subsumes_p(Dag,G,Glb) end,Glbs) of
        true ->
            case length(Glbs) of
                0 ->
                    [Glb];
                _ ->
                    Glbs
            end;
        false ->
            add_con_if_satisfies(
              Dag,
              Glb,
              Glbs,
              fun(Elem) ->
                      subsumes_p(Dag,Glb,Elem)
              end)
    end.
%%
%%
add_con_if_satisfies(_Dag,Con,Cons,Fun) ->
    %%?LOG(?DEBUG,"checking adding con to list ~p ~p ~n",[label(Dag,Con),labels(Dag,Cons)]),
    ConsToRemove = 
        lists:foldl(fun(Elem, Acc) ->
                            case Fun(Elem) of
                                true -> lists:append(Acc, [Elem]); 
                                _ -> Acc
                            end
                    end,[],Cons),
    NewCons = lists:subtract(Cons,ConsToRemove),
    %%?LOG(?DEBUG,"ok, adding concept ~p  to ~p ~n",[label(Dag,Con),labels(Dag,NewCons)]),            
    lists:append(NewCons,[Con]).        
%%
%%
create_inferred_facts(Dag,Lubs,Glbs,Concept) ->
    Arrow = get(<<"children">>),
    CheckLubs = map(fun(Lub) ->
                            case is_parent(Dag,Concept,Lub) of
                                true ->
                                    false;
                                _ ->
                                    add_edge(Dag,Concept,Lub,get(<<"children">>)),
                                    true
                            end end,Lubs),
    CheckGlbs = map(fun(Glb) ->
                            case is_parent(Dag,Glb,Concept) of
                                true ->
                                    false;
                                _ -> 
                                    
                                    ConsToRemove =
                                        lists:foldl(fun(Edge,Acc) ->
                                                            {_,_,V2,Label} = digraph:edge(Dag,Edge),
                                                            case (Label == Arrow)
                                                                andalso
                                                                (V2 /= Concept)
                                                                andalso
                                                                is_parent(Dag,Glb,V2) of
                                                                true ->
                                                                    Acc ++ [V2];
                                                                _ ->
                                                                    Acc
                                                            end
                                                    end,[],digraph:edges(Dag,Concept)),
                                    map(fun(Con) ->
                                                remove_parent(Dag,Glb,Con)
                                        end,ConsToRemove),
                                    add_edge(Dag,Glb,Concept,get(<<"children">>)),
                                    true
                            end end,Glbs),                                    
    case any(fun(Elem) ->
                     case Elem of
                         true ->
                             true;
                         _ ->
                             false
                     end end,CheckLubs) of
        true ->
            relabel_vertex(Dag,Concept,{extract_key(Dag,Concept),classified_modified});
        _ ->
            case any(fun(Elem) ->
                             case Elem of
                                 true ->
                                     true;
                                 _ ->
                                     false
                             end end,CheckGlbs) of
                true ->
                    relabel_vertex(Dag,Concept,{extract_key(Dag,Concept),classified_modified});
                _ ->
                    relabel_vertex(Dag,Concept,{extract_key(Dag,Concept),classified})
            end
    end.
%%
%% compare the definitions of two concepts
%% this is where all the logical work in classification is
is_greater(Dag,Subsumer,Subsumee) ->
    SupKey = extract_key(Dag,Subsumer),
    SubKey = extract_key(Dag,Subsumee),
    DagCask = get(<<"cask">>),
    SupDef = dag:get_targets(SupKey,DagCask),
    SubDef = dag:get_targets(SubKey,DagCask),
    %%?LOG(?DEBUG,"the definition of ~p is ~p ~n",[SupKey,SupDef]), 
    %%?LOG(?DEBUG,"the definition of  ~p is  ~p ~n",[SubKey,SubDef]),
    %% all works even if SupDef is empty, as it should given all is vacuously true
    %% it is a thing of beauty to be able to state something so succintly
    case SupDef == [] of
        true ->
            false;
        false ->
            case SubDef == [] of
                true ->
                    false;
                false ->
                    all(fun(SupRel) ->
                                any(fun(SubRel) ->
                                            rel_subsumes_p(Dag,SupRel,SubRel)
                                    end,SubDef)
                        end,SupDef)
            end
    end.
    
   
%%
%% subsumes_p merely tests if the two concepts are already in the subsumes
%% relation, either directly or transitively 
subsumes_p(Dag,Subsumer,Subsumee) ->
    %% This needs to only look for path thru "isa" links
    case Subsumer == Subsumee of
        true ->
            true;
        false ->
            case is_parent(Dag,Subsumee,Subsumer) of
                false -> 
                    false;
                _ ->
                    %%?LOG(?DEBUG,"subsumes_p found a path ~p ~p ~n",[label(Dag,Subsumee),label(Dag,Subsumer)]),
                    true
            end
    end.
%%
%%
rel_subsumes_p(Dag,SupRel,SubRel) ->
    %%?LOG(?DEBUG,"checking role vals ~p ~p ~n",[SupRel,SubRel]),
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
remove_parent(Dag,Con,Parent) ->
    digraph:delete_path(Dag,Con,Parent).
   
%%
%%
store_new_facts(Dag, Concept) ->
    ?LOG(?DEBUG,"This concept's status ~p ~n",[label(Dag,Concept)]),
    DagCask = get(<<"cask">>),
    map(fun(Edge) ->
                {_,Source,Target,Arrow} = digraph:edge(Dag,Edge),
                SrcKey = extract_key(Dag,Source),
                TarKey = extract_key(Dag,Target),
                ?LOG(?DEBUG,"Adding edge to cask: {~p ~p ~p} ~n",[SrcKey,Arrow,TarKey]),
                dag:add_edge({SrcKey,Arrow,TarKey},DagCask)
        end,digraph:edges(Dag,Concept)),    
    ok.
%%
%% creates a new vertex with given label
add_labeled_vertex(Dag,Label) ->
    V = add_vertex(Dag),
    add_vertex(Dag,V,Label).
%%
relabel_vertex(Dag,Vertex,Label) ->
    add_vertex(Dag,Vertex,Label).    
%%
%%
is_classified(Dag, Concept) ->
    Label = element(2,label(Dag, Concept)),
    %%?LOG(?DEBUG,"checking is classified for ~p ~n",[Label]),
    (Label == classified) orelse (Label == classified_modified).
%%
%%
is_classified_modified(Dag, Concept) ->
    element(2,label(Dag, Concept)) == classified_modified.
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
    ?LOG(?DEBUG,"classifying concept ~p ~n",[label(Dag, Concept)]).

labels(Dag, Cons) ->
    map(fun(Con) ->
                label(Dag,Con)
        end,Cons).

label(Dag, Concept) ->
    element(2,vertex(Dag, Concept)).
%%
%%
sl_flatten([]) -> [];
sl_flatten([[]]) -> [];
sl_flatten([H | T]) ->
    case H == [] of
        true ->
            sl_flatten(T);
        false ->
            [hd(H) | sl_flatten(T)]
    end.


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
