%%%-------------------------------------------------------------------
%%% File    : load_triple_store.erl
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
%%% Bitstore, Copyright (C) 2009-2010   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(load_triple_store).
-author('dionne@dionne-associates.com').
%%
%%
%%
-export([load_dag/1]).

-import(triple_store, [insert_tuple/4, init/1]).

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

store_triple(TripNodeIds,Table) ->
    case TripNodeIds of
        [{_, SubId},
         {_, PredId},
         {_, ObjId}] ->
            insert_tuple(SubId, PredId, ObjId, Table);
        _ -> []
    end.

build_triples(File, NodeDict, Table) ->
    case io:get_line(File,'') of
        eof -> NodeDict;
        LS -> 
            Triple = getTriple(element(2,erl_scan:string(LS))),
            {TripleNodeIdPairs, NewNodeDict} = assign_ids(Triple, NodeDict, []),
            store_triple(TripleNodeIdPairs, Table),
            build_triples(File, NewNodeDict, Table)
    end.

%% load_dag takes a text file, each line of which specifies a source/arrow/target
%% triplet, assigns a unique id to each item and stores the triplets
%% in a mnesia table
%% 
load_dag(Dag) ->
    io:format("Loading Dag: ",[]),
    File = element(2,file:open(Dag, [read])),
    {atom, 1, Table} = hd(element(2, erl_scan:string(io:get_line(File,'')))),
    init(Table),
    NodeDict = build_triples(File,dict:new(),Table),
    io:format("Dag Built ~n",[]),
    NodeDict.
    
    


                                                                 
    
