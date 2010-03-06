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
%% API
-export([build_dag/1,
         find_or_create_pid/2,
         id/1,
         persist_dag/2,
         print_dag/1,
         add_edge/3,
         remove_edge/3,
         get_edge_targets/2,
         get_edge_sources/3,
         get_targets/2,
         get_sources/3,         
         path_exists/2,
         dag_node/2]).

-import(triple_store, [all_triples/1, insert_tuple/4, delete_tuple/4, 
                       get_column/4, get_projection/3]).
-import(lists, [foldl/3, map/2, member/2, delete/2, any/2]).
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------

%% from a named mnesia table construct a dag as 
%% a collection of processes, one for each node,
%% each containing a dictionary mapping labelled arrows
%% to lists of target nodes 
build_dag(Table) ->
    %%io:format("Loading dag from mnesia ~n",[]),
    Nodes = dict:new(),
    %%Nodes1 = dict:store(<<"0">>,Table,Nodes),
    foldl(fun({Source,Arrow,Target}, Acc) ->
                      {SourcePid, NewAcc1} = find_or_create_pid(Source,Acc),
                      {TargetPid, NewAcc2} = find_or_create_pid(Target,NewAcc1),
                      SourcePid ! {add, Arrow, TargetPid},
                      %%io:format("size of nodes is ~p ~n",[dict:size(NewAcc2)]),
                      NewAcc2
              end, Nodes, all_triples(Table)).

persist_dag(Dag, Table) ->
    AllNodes = dict:to_list(Dag),
    map(fun({_, NodePid}) ->
                      NodePid ! {persist, Table, self()},
                receive
                    ok -> ok;
                    _ -> ok
                end
              end, AllNodes).

print_dag(Dag) ->
    AllNodes = dict:to_list(Dag),
    map(fun({_, NodePid}) ->
                NodePid ! {print, self()},
                receive
                    ok ->
                        ok;
                    _ -> ok
                end
        end, AllNodes).

get_edge_targets(Dag, {SubId, PredId}) ->
    {SubPid, _} = find_or_create_pid(SubId, Dag),
    SubPid ! {edge_targets, PredId, self()},
    receive
        {edge_targets, Nodes} -> Nodes
    end.

get_edge_sources(Dag, {ObjId, PredId}, TabName) ->
    {ObjPid, _} = find_or_create_pid(ObjId, Dag),
    ObjPid ! {edge_sources, PredId, TabName, self()},
    receive
        {edge_sources, Nodes} -> Nodes
    end.

get_targets(Dag, SubId) ->
    try
        {SubPid, _} = find_or_create_pid(SubId, Dag),
        SubPid ! {edges, self()},
        receive
            {edges, ConceptDef} ->
                ConceptDef
        end
    catch
        error:function_clause ->
            io:format("a bug ~p ~n",[erlang:get_stacktrace()]),
            []
    end.

get_sources(Dag, ObjId, TabName) -> 
    try
        {ObjPid, _} = find_or_create_pid(ObjId, Dag),
        ObjPid ! {in_edges, TabName, self()},
        receive
            {in_edges, ConceptDef} ->
                ConceptDef
        end
    catch
        error:function_clause ->
            io:format("a bug ~p ~n",[erlang:get_stacktrace()]),
            []
    end.

path_exists(Dag, {SubId, PredId, TargetId}) ->
    {SubPid, _} = find_or_create_pid(SubId, Dag),
    {TargetPid, _} = find_or_create_pid(TargetId, Dag),
    SubPid ! {can_reach, PredId, TargetPid, self()},
    receive
        {can_reach, Bool} ->
            Bool
    end.           

add_edge(Dag, {SubId, PredId, ObjId}, DbName) ->
    {SubPid, Dag1} = find_or_create_pid(SubId, Dag),
    {ObjPid, Dag2} = find_or_create_pid(ObjId, Dag1),
    SubPid ! {add, PredId, ObjPid},
    SubPid ! {add_tuple, PredId, ObjPid, DbName},
    Dag2.

remove_edge(Dag, {SubId, PredId, ObjId}, DbName) ->
    {SubPid, Dag1} = find_or_create_pid(SubId, Dag),
    {ObjPid, Dag2} = find_or_create_pid(ObjId, Dag1),
    SubPid ! {remove, PredId, ObjPid},
    SubPid ! {remove_tuple, PredId, ObjPid, DbName},
    Dag2.                      
%%====================================================================
%% Internal functions
%%====================================================================
find_or_create_pid(Id,Nodes) ->
    case dict:find(Id,Nodes) of
        {ok, Pid} ->
            %%io:format("Pid found in call to find_or_create_pid ~n",[]),
            {Pid, Nodes};
        error ->
            Pid = spawn(?MODULE, dag_node, [Id, dict:new()]),
            NewNodes = dict:store(Id, Pid, Nodes),
            {Pid, NewNodes}            
    end.

id(Pid) ->
    Pid ! {name, self()},
    receive
        {my_name, Id} ->
            Id
    end.            

dag_node(Id, Dict) ->
    receive
        {print, CallerPid} ->
            AllEdges = dict:to_list(Dict),
            map(fun({_Pred, Vals}) ->
                        %%io:format(" predicate: ~s ~n",[Pred]),
                        map(fun(Val) ->
                                    io:format("  target: ~s ~n",[id(Val)])
                            end, Vals)
                end, AllEdges),
            CallerPid ! ok,
            dag_node(Id, Dict);
        %% send the id of this node to another process
        {name, Pid} -> Pid ! {my_name, Id},
                  dag_node(Id,Dict);
        {edge_targets, ArrowId, CallerPid} ->
            case dict:find(ArrowId, Dict) of
                {ok, TargetList} -> CallerPid ! {edge_targets, map(fun id/1, TargetList)};
                _ -> CallerPid ! []
            end,
            dag_node(Id, Dict);
        %% to get in bound links we need to go to mnesia
        {edge_sources, ArrowId, TableName, CallerPid} ->
            SourceIds = get_column(ArrowId, Id, source, TableName), 
            CallerPid ! {edge_sources, map(fun(Elem) ->
                                                   element(1,Elem)
                                                       end,
                                           SourceIds)},
            dag_node(Id, Dict);
        %%
        {edges, CallerPid} ->
            AllEdges = dict:to_list(Dict),
            Result = map(fun({Key,Values}) ->
                                          {Key, map(fun id/1, Values)}
                                  end, AllEdges),
            CallerPid ! {edges, Result},
            dag_node(Id, Dict);
        {in_edges, TableName, CallerPid} ->
            InEdges = get_projection(Id, target, TableName),
            CallerPid ! {in_edges, InEdges},
            dag_node(Id, Dict);            
        %% add labeled edge to another process
        {add, ArrowId, TargetPid} ->
            NewDict = 
                case dict:find(ArrowId,Dict) of
                    {ok, _} ->
                        dict:append(ArrowId,TargetPid,Dict);
                    error -> dict:store(ArrowId,[TargetPid],Dict)
                end,
            dag_node(Id, NewDict);
        {add_tuple, ArrowId, TargetPid, Table} ->
            insert_tuple(Id, ArrowId, id(TargetPid), Table),
            dag_node(Id, Dict);
        %% delete labeled edge to another process
        {remove, ArrowId, TargetPid} ->
            NewDict = 
                case dict:find(ArrowId,Dict) of
                    {ok, TargetList} ->
                        NewTargetList = case member(TargetPid, TargetList) of
                                            true -> 
                                                delete(TargetPid, TargetList);
                                            false ->
                                                TargetList
                                        end,                                
                        dict:store(ArrowId, NewTargetList, Dict);
                    error -> Dict
                end,
            dag_node(Id, NewDict);
        {remove_tuple, ArrowId, TargetPid, Table} ->
            delete_tuple(Id, ArrowId, id(TargetPid), Table),
            dag_node(Id, Dict);
        %% serialize node back out to mnesia table
        {persist, Table, CallerPid} ->
            AllEdges = dict:to_list(Dict),
            map(fun({Pred, Vals}) ->
                        map(fun(Val) ->
                                    insert_tuple(Id,Pred,id(Val),Table)
                            end, Vals)
                end, AllEdges),
            CallerPid ! ok,
            dag_node(Id, Dict);                    
        %% is node connected to another through a specific labelled
        %% edge
        {can_reach, ArrowId, TargetPid, Pid} ->
            case dict:find(ArrowId,Dict) of
                {ok, AdjNodes} ->
                    case member(TargetPid,AdjNodes) of
                        true -> Pid ! {can_reach, true};
                        false ->
                            case any(fun(Node) ->
                                             Node ! {can_reach,
                                                     ArrowId, TargetPid,
                                                     self()},
                                             receive {can_reach, true} -> 
                                                     true;
                                                     {can_reach, false} -> false
                                             end
                                     end, AdjNodes) of
                                true -> Pid ! {can_reach, true};
                                false -> Pid ! {can_reach, false}
                            end
                    end;
                error -> Pid ! {can_reach, false}
            end,                                                  
            dag_node(Id, Dict)
    end.

    
