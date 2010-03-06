%%%-------------------------------------------------------------------
%%% File    : triple_store.erl
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
%%% Created :  11 Dec 2009 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% bitstore, Copyright (C) 2009-2010   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(triple_store).
-author('dionne@dionne-associates.com').
%%
%%
%%
-export([init/1, delete/1, insert_tuple/4,
         delete_tuple/4,
         all_triples/1, all_arrows/1, 
         get_source/2, get_relation/2, get_target/2,
         get_column/4, get_projection/3,
         get_trans_closure/4]).

-include_lib("stdlib/include/qlc.hrl").
-include("triple.hrl").

init(Table) ->
    mnesia:create_table(Table,
                        [{disc_copies,
			  [node()]},
			 {type, bag},
                         {record_name, triple},
			 {attributes,
			  record_info(fields, triple)}]).

delete(Table) ->
    mnesia:del_table_copy(Table, node()).

insert_tuple(Source, Arrow, Target, Table) ->
    mnesia:transaction(fun() ->
		  mnesia:write(Table,#triple{source = Source,
				arrow = Arrow,
				target = Target},write)
	  end).

delete_tuple(Source, Arrow, Target, Table) ->
    mnesia:transaction(fun() ->
		  mnesia:delete(Table,#triple{source = Source,
				arrow = Arrow,
				target = Target},write)
	  end).

all_triples(Table) ->
    case lists:member(Table, mnesia:system_info(tables)) of
        true -> ok;
        _ -> init(Table)
    end,
    F = fun() ->
                Q = qlc:q([{Edge#triple.source, Edge#triple.arrow,
                            Edge#triple.target} ||
                              Edge <- mnesia:table(Table)], {unique, true}),
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).

all_arrows(Table) ->
    F = fun() ->
                Q = qlc:q([Edge#triple.arrow 
			   || Edge <- mnesia:table(Table)], {unique, true}),
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).

get_projection(Match, Column, Table) ->
    Q = case Column of
            arrow -> 
		qlc:q([{Edge#triple.source, Edge#triple.target}
		       || Edge <- mnesia:table(Table),
			  Edge#triple.arrow == Match]);
            source ->
		qlc:q([{Edge#triple.arrow, Edge#triple.target}
		       || Edge <- mnesia:table(Table),
			  Edge#triple.source == Match]);
            target -> 
		qlc:q([{Edge#triple.source, Edge#triple.arrow}
		       || Edge <- mnesia:table(Table),
			  Edge#triple.target == Match])
        end,
    F = fun() ->
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).

get_column(Match1, Match2, Column, Table) ->
    Q = case Column of
            arrow ->
                qlc:q([{Edge#triple.arrow} ||
                          Edge <- mnesia:table(Table),
                          Edge#triple.source == Match1,
                          Edge#triple.target == Match2]);
            source ->
                qlc:q([{Edge#triple.source} ||
                          Edge <- mnesia:table(Table),
                          Edge#triple.arrow == Match1,
                          Edge#triple.target == Match2]);
            target ->
                qlc:q([{Edge#triple.target} ||
                          Edge <- mnesia:table(Table),
                          Edge#triple.source == Match1,
                          Edge#triple.arrow == Match2])
        end,
    F = fun() ->
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).
                

get_relation(Relation, Table) ->
    get_projection(Relation, arrow, Table).

get_source(Source, Table) ->
    get_projection(Source, source, Table).

get_target(Target, Table) ->
    get_projection(Target, target, Table).

get_trans_closure(Source, Arrow, VisitedNodes, Table) ->
    F = fun() ->
                Q = qlc:q([Edge#triple.target
                           || Edge <- mnesia:table(Table),
                              Edge#triple.source == Source,
                              Edge#triple.arrow == Arrow]),
                qlc:e(Q)
        end,
    Targets = element(2, mnesia:transaction(F)),
    case Targets of
        [] -> VisitedNodes;
        _ -> lists:foldl(fun(Elem, Acc) ->
                                 AlreadyVisited = lists:member(Elem, VisitedNodes),
                                 case AlreadyVisited of
                                     true -> VisitedNodes;
                                     false -> get_trans_closure(Elem,Arrow,[Elem | Acc],Table)
                                 end
                         end, VisitedNodes, Targets)
    end.
    


                                                                 
    
