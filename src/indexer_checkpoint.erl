%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%
%%  Original copyright: "(c) 2007 armstrongonsoftware"
%%
%% 12/1/2009 modified to store checkpoint in CouchDB index database
%%
%%% Copyright (C) 2009   Dionne Associates, LLC.
%%---
-module(indexer_checkpoint).

-export([init/2, resume/1, checkpoint/2, test/0]).

-include("bitstore.hrl").
%%-import(filelib, [is_file/1]).

%%% init(DbIndexName, Term) -> true.   %% define initialize checkpoints in the index db
%%  resume(DbIndexName) -> {Check, X}  %% Check is used in the *next* call to checkpoint
%%                             %% X is a term
%%  checkpoint(Check, X) ->    %% Set a new checkpoint
%%     Check'
init(Db, X) ->
    One = <<"check1">>,
    Two = <<"check2">>,
    case already_exists(One, Db) or already_exists(Two, Db) of
	true ->
	    exit(eBadInit);
	false ->
	    checkpoint({Db, 1}, X),
	    checkpoint({Db, 2}, X)
    end.

already_exists(DocId, Db) ->
    CheckExists = indexer_couchdb_crawler:lookup_doc_bitcask(DocId, Db),
    case CheckExists of
        not_found ->
            false;
        {ok, _} ->
            true
    end.

resume(Db) ->
    R1 = recover(Db, <<"check1">>),
    R2 = recover(Db, <<"check2">>),
    case {R1, R2} of
	{error, error}               -> error;
	{error, _}                   -> {{Db, 1}, R2};
	{_, error}                   -> {{Db, 2}, R1};
	{{T1,X},{T2,_}} when T1 > T2 -> {{Db, 2}, X};
	{_,{_,X}}                    -> {{Db, 1}, X}
    end.

recover(Db, ChkpNameBin) ->
    {ok, ChkpDoc} = indexer_couchdb_crawler:lookup_doc_bitcask(ChkpNameBin, Db),
    proplists:get_value(<<"chkp">>,element(1,ChkpDoc)).

checkpoint({Db, Next}, X) ->
    %%?LOG(?DEBUG, "creating checkpoint for:~p ~p ~p ~n", [DbIndexName, Next, X]),
    DocId = list_to_binary("check" ++ integer_to_list(Next)),
    Time = now(),
    B = {Time, X},
    indexer_couchdb_crawler:store_chkp(DocId, B, Db),
    {Db, 3-Next}.

test() ->
    Cont = indexer_couchdb_crawler:start(<<"fti">>,[{reset, <<"fti-idx">>}]),
    init(<<"fti-idx">>,Cont).



    

