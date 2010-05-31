%%%-------------------------------------------------------------------
%%% File    : dag.erl
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
%%% Created :  05 Dec 2009 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% bitstore, Copyright (C) 2009-2010   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(dag).
-author('dionne@dionne-associates.com').
%%
%%
%% {Links, References}
%% 
%% where Links = [{arr1,[target_node_ids]}]
%% and references = [{arr1,[source_node_ids]}]
%% 
%% API
-export([create_or_open_dag/2,
         add_edge/2,
         remove_edge/2,
         get_edge_targets/2,
         get_edge_sources/2,
         get_targets/2,
         get_sources/2,
         get_roots/2,
         path_exists/2,
         close_dag/1]).
%%
-include("bitstore.hrl").
-import(bitcask, [open/2,get/2,put/3,fold/3,close/1]).
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
%%
%%
create_or_open_dag(DbName, CreateP) ->
    DsName = DbName ++ "-dag",
    case filelib:is_dir(DsName) of
        true ->
            case CreateP of
                true ->
                    ?LOG(?DEBUG, "removing dag store for ~p ~n",[DsName]),
                    os:cmd("rm -rf " ++ DsName);
                false ->
                    ok
            end;
        false ->
            ok
    end,
    open(DsName, [read_write, {max_file_size, 100000000}]).
%%
%%
close_dag(Dag) ->
    close(Dag).
%%
%%
add_edge({Source, Arrow, Target},Dag) ->
    SourceNode = find_or_create_node(Source,Dag), 
    TargetNode = find_or_create_node(Target,Dag),
    NewLinks = add_arrow(Arrow,Target,get_links(SourceNode)),
    NewRefs = add_arrow(Arrow,Source,get_references(TargetNode)),
    store_node(Source,{NewLinks,get_references(SourceNode)},Dag),
    store_node(Target,{get_links(TargetNode),NewRefs},Dag).
%%
%%    
remove_edge({Source, Arrow, Target},Dag) ->
    SourceNode = find_or_create_node(Source,Dag), 
    TargetNode = find_or_create_node(Target,Dag),
    NewLinks = remove_arrow(Arrow,Target,get_links(SourceNode)),
    NewRefs = remove_arrow(Arrow,Source,get_references(TargetNode)),
    store_node(Source,{NewLinks,get_references(SourceNode)},Dag),
    store_node(Target,{get_links(TargetNode),NewRefs},Dag).
%%
%%
get_edge_targets({Source, Arrow},Dag) ->
    case get_node(Source,Dag) of
        [] -> [];
        SourceNode ->
            Edges = proplists:lookup(Arrow,get_links(SourceNode)),
            case Edges of
                none ->
                    none;
                {Arrow, Targets} -> Targets
            end
    end.
%%
%%
get_edge_sources({Target, Arrow},Dag) ->
    case get_node(Target,Dag) of
        [] -> [];
        TargetNode ->
            Edges = proplists:lookup(Arrow,get_references(TargetNode)),
            case Edges of
                none ->
                    none;
                {Arrow, Sources} -> Sources
            end
    end.
%%
%%
get_targets(Source,Dag) ->
    case get_node(Source,Dag) of
        [] ->
            [];
        SourceNode ->
            get_links(SourceNode)
    end.
%%
%%
get_sources(Target,Dag) ->
    case get_node(Target,Dag) of
        [] ->
            [];
        TargetNode ->
            get_references(TargetNode)
    end.
%%
%%
get_roots(Arrow,Dag) -> 
    fold(Dag,
                 fun(K,V,Acc) ->
                         Node = binary_to_term(V),
                         Edges = proplists:lookup(Arrow,get_links(Node)),
                         case Edges of
                             none ->
                                 InEdges = proplists:lookup(Arrow,get_references(Node)),
                                 case InEdges of
                                     none -> Acc;
                                     _ -> Acc ++ [K]
                                 end;
                             _ -> Acc
                         end
                 end,[]).
%%
path_exists({_Source,_Arrow,_Target},_Dag) ->
    ok.
    


%% internal private functions
store_node(NodeId,Node,Dag) ->
    put(Dag,NodeId,term_to_binary(Node)).
%%
%%
get_links(Node) ->
    element(1,Node).
%%
%%
get_references(Node) ->
    element(2,Node).
%%
%%
%% 
get_node(NodeId, Dag) ->
    case get(Dag, NodeId) of
        not_found ->
            %% return empty if node doesn't exist, saves call
            [];
        {ok, Node} -> binary_to_term(Node)
    end.
%%
%%
find_or_create_node(NodeId,Dag) ->
    case get_node(NodeId, Dag) of
        [] ->
            NewNode = {[],[]},
            store_node(NodeId,NewNode,Dag),
            NewNode;
        Node -> Node
    end.
%%
%%
add_arrow(ArrowId,NodeId,Edges) -> 
    case proplists:lookup(ArrowId,Edges) of
        none ->
            lists:append([{ArrowId,[NodeId]}], Edges);
        {ArrowId, NodeList} ->
            case lists:member(NodeId,NodeList) of
                true ->
                    Edges;
                _ ->
                    NewEdges = proplists:delete(ArrowId,Edges),
                    lists:append(NewEdges,[{ArrowId,lists:append(NodeList,[NodeId])}])
            end
    end.    
%%
%%
remove_arrow(ArrowId,NodeId,Edges) -> 
    case proplists:lookup(ArrowId,Edges) of
        none ->
            Edges;
        {ArrowId, NodeList} ->
            NewEdges = proplists:delete(ArrowId,Edges),
            case length(NodeList) of
                1 ->
                    NewEdges;
                _ -> 
                    lists:append(NewEdges,
                                 [{ArrowId,lists:delete(NodeId,NodeList)}])
            end
    end.    
%% 
%% EUnit tests
%% 
-ifdef(TEST).
%%
add_edge_test() ->
    Dag = create_or_open_dag("onty",true),
    add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    ?assert(length(get_links(get_node(<<"001">>,Dag))) == 1),
    close_dag(Dag).
%%
remove_edge_test() ->
    Dag = create_or_open_dag("onty",true),
    add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    ?assert(length(get_links(get_node(<<"001">>,Dag))) == 1),
    remove_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    ?assert(length(get_links(get_node(<<"001">>,Dag))) == 0),
    close_dag(Dag).
%%
get_edge_targets_test() ->
    Dag = create_or_open_dag("onty",true),
    add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    ?assert(length(get_links(get_node(<<"001">>,Dag))) == 1),
    ?assert(length(get_edge_targets({<<"001">>,<<"002">>},Dag)) == 1),
    [<<"003">>] = get_edge_targets({<<"001">>,<<"002">>},Dag),
    close_dag(Dag).
%%
get_edge_sources_test() ->
    Dag = create_or_open_dag("onty",true),
    add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    ?assert(length(get_references(get_node(<<"003">>,Dag))) == 1),
    ?assert(length(get_edge_sources({<<"003">>,<<"002">>},Dag)) == 1),
    [<<"001">>] = get_edge_sources({<<"003">>,<<"002">>},Dag),
    close_dag(Dag).
%
get_targets_test() ->
    Dag = create_or_open_dag("onty",true),
    add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    ?assert(length(get_targets(<<"001">>,Dag)) == 1),
    [{<<"002">>,[<<"003">>]}] = get_targets(<<"001">>,Dag),
    close_dag(Dag).
%%
get_sources_test() ->
    Dag = create_or_open_dag("onty",true),
    add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    ?assert(length(get_sources(<<"003">>,Dag)) == 1),
    [{<<"002">>,[<<"001">>]}] = get_sources(<<"003">>,Dag),
    close_dag(Dag).
%%
get_roots_test() ->
    Dag = create_or_open_dag("onty",true),
    add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    ?assert(length(get_roots(<<"002">>,Dag)) == 1),
    [<<"003">>] = get_roots(<<"002">>,Dag),
    close_dag(Dag).
%%
get_multiple_roots_test() ->
    Dag = create_or_open_dag("onty",true),
    add_edge({<<"001">>,<<"002">>,<<"003">>},Dag),
    add_edge({<<"004">>,<<"002">>,<<"001">>},Dag),
    add_edge({<<"001">>,<<"002">>,<<"005">>},Dag),
    ?assert(length(get_roots(<<"002">>,Dag)) == 2),
    Roots = get_roots(<<"002">>,Dag),
    ?assert(lists:member(<<"003">>,Roots)),
    ?assert(lists:member(<<"005">>,Roots)),
    close_dag(Dag).
    
%%
-endif.
