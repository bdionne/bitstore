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
-export([init/0, delete/0, insert_tuple/3,
         all_triples/0, all_arrows/0, 
         get_source/1, get_relation/1, get_target/1,
         get_column/3,
         get_trans_closure/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("triple.hrl").

init() ->
    mnesia:create_table(triple,
                        [{disc_copies,
			  [node()]},
			 {type, bag},
			 {attributes,
			  record_info(fields, triple)}]).

delete() ->
    mnesia:del_table_copy(triple, node()).

insert_tuple(Source, Arrow, Target) ->
    mnesia:transaction(fun() ->
		  mnesia:write(#triple{source = Source,
				arrow = Arrow,
				target = Target})
	  end).

all_triples() ->
    F = fun() ->
                Q = qlc:q([{Edge#triple.source, Edge#triple.arrow,
                            Edge#triple.target} ||
                              Edge <- mnesia:table(triple)], {unique, true}),
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).

all_arrows() ->
    F = fun() ->
                Q = qlc:q([Edge#triple.arrow 
			   || Edge <- mnesia:table(triple)], {unique, true}),
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).

get_projection(Match, Column) ->
    Q = case Column of
            arrow -> 
		qlc:q([{Edge#triple.source, Edge#triple.target}
		       || Edge <- mnesia:table(triple),
			  Edge#triple.arrow == Match]);
            source ->
		qlc:q([{Edge#triple.arrow, Edge#triple.target}
		       || Edge <- mnesia:table(triple),
			  Edge#triple.source == Match]);
            target -> 
		qlc:q([{Edge#triple.source, Edge#triple.arrow}
		       || Edge <- mnesia:table(triple),
			  Edge#triple.target == Match])
        end,
    F = fun() ->
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).

get_column(Match1, Match2, Column) ->
    Q = case Column of
            arrow ->
                qlc:q([{Edge#triple.arrow} ||
                          Edge <- mnesia:table(triple),
                          Edge#triple.source == Match1,
                          Edge#triple.target == Match2]);
            source ->
                qlc:q([{Edge#triple.source} ||
                          Edge <- mnesia:table(triple),
                          Edge#triple.arrow == Match1,
                          Edge#triple.target == Match2]);
            target ->
                qlc:q([{Edge#triple.target} ||
                          Edge <- mnesia:table(triple),
                          Edge#triple.source == Match1,
                          Edge#triple.arrow == Match2])
        end,
    F = fun() ->
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).
                

get_relation(Relation) ->
    get_projection(Relation, arrow).

get_source(Source) ->
    get_projection(Source, source).

get_target(Target) ->
    get_projection(Target, target).

get_trans_closure(Source, Arrow, VisitedNodes) ->
    F = fun() ->
                Q = qlc:q([Edge#triple.target
                           || Edge <- mnesia:table(triple),
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
                                     false -> get_trans_closure(Elem,Arrow,[Elem | Acc])
                                 end
                         end, VisitedNodes, Targets)
    end.
    


                                                                 
    
