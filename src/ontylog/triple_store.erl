%%%-------------------------------------------------------------------
%%% File    : triple_store.erl
%%% Author  : Robert Dionne
%%% Description : 
%%%
%%% Created :  11 Dec 2009 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% bitstore, Copyright (C) 2009   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(triple_store).
-author('dionne@dionne-associates.com').
%%
%%
%%
-export([init/0, delete/0, insert_tuple/3, all_arrows/0, 
         get_source/1, get_relation/1, get_target/1,
         get_trans_closure/3]).

-include_lib("stdlib/include/qlc.hrl").
-include("triple.hrl").

init() ->
    mnesia:create_table(triples,
                        [{disc_copies,
			  [node()]},
			 {type, bag},
			 {attributes,
			  record_info(fields, triple)}]).

delete() ->
    mnesia:del_table_copy(triples, node()).

insert_tuple(Source, Arrow, Target) ->
    mnesia:transaction(fun() ->
		  mnesia:write(#triple{source = Source,
				arrow = Arrow,
				target = Target})
	  end).

all_arrows() ->
    F = fun() ->
                Q = qlc:q([Edge#triple.arrow 
			   || Edge <- mnesia:table(dag)], {unique, true}),
                qlc:e(Q)
        end,
    element(2, mnesia:transaction(F)).

get_projection(Match, Column) ->
    Q = case Column of
            arrow -> 
		qlc:q([{Edge#triple.source, Edge#triple.target}
		       || Edge <- mnesia:table(dag),
			  Edge#triple.arrow == Match]);
            source ->
		qlc:q([{Edge#triple.arrow, Edge#triple.target}
		       || Edge <- mnesia:table(dag),
			  Edge#triple.source == Match]);
            target -> 
		qlc:q([{Edge#triple.source, Edge#triple.arrow}
		       || Edge <- mnesia:table(dag),
			  Edge#triple.target == Match])
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

get_trans_closure(Source, Arrow, []) ->
    F = fun() ->
                Q = qlc:q([Edge#triple.target
			   || Edge <- mnesia:table(dag),
			      Edge#triple.source == Source,
                              Edge#triple.arrow == Arrow]),
                qlc:e(Q)
        end,
    Targets = element(2, mnesia:transaction(F)),
    case Targets of
        [] -> [];
        _ -> lists:foldl(fun(Elem, Acc) ->
                                   get_trans_closure1(Elem,Arrow,Acc)
                           end, [], Targets)
    end.
    

get_trans_closure1(Source, Arrow, VisitedNodes) ->

    AlreadySeen = lists:member(Source, VisitedNodes),

    case AlreadySeen of 
        true -> VisitedNodes;
        _ -> NewVisitedNodes = [Source | VisitedNodes],
             F = fun() ->
                         Q = qlc:q([Edge#triple.target
				    || Edge <- mnesia:table(dag),
				       Edge#triple.source == Source,
				       Edge#triple.arrow == Arrow]),
                         qlc:e(Q)
                 end,
             Targets = element(2, mnesia:transaction(F)),
             case Targets of
                 [] -> NewVisitedNodes;
                 _ -> lists:foldl(fun(Elem, Acc) ->
                                        get_trans_closure1(Elem,Arrow,Acc)
                                end, NewVisitedNodes, Targets)
             end
    end.
    


                                                                 
    
