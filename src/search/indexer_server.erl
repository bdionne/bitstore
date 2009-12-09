%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%
%% Original copyright: (c) 2007 armstrongonsoftware
%% 
%%---
-module(indexer_server).
-author('Joe Armstrong').

%% This code was modified considerably to integrate with couchdb
%% but still retains the original ideas from the text book
-author('Bob Dionne').

-export([next_docs/1,
         total_docs/1,
         db_name/1,
         get_changes/1,
	 ets_table/1, 
	 checkpoint/1,
         checkpoint/3,
	 schedule_stop/1,
	 search/2,
         write_index/3,
         write_bulk_indices/2,
         delete_index/3,
	 should_i_stop/1,
	 stop/1]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-import(filename, [join/2]).
-include("indexer.hrl").

schedule_stop(Pid) ->
    gen_server:call(Pid, schedule_stop, infinity).

should_i_stop(Pid) ->
    gen_server:call(Pid, should_i_stop).

stop(Pid) ->
     gen_server:cast(Pid, stop).

next_docs(Pid)   -> gen_server:call(Pid, next_docs, infinity).

total_docs(Pid) ->
    gen_server:call(Pid, total_docs).

db_name(Pid) ->
    gen_server:call(Pid, db_name).

get_changes(Pid) ->
     gen_server:call(Pid, changes, infinity).

checkpoint(Pid) -> gen_server:call(Pid, checkpoint).
checkpoint(Pid, changes, LastSeq) ->
    gen_server:call(Pid, {checkpoint, changes, LastSeq}).

ets_table(Pid)  -> gen_server:call(Pid, ets_table).
    
search(Pid, Str)  -> gen_server:call(Pid, {search, Str}, infinity).

write_index(Pid, Key, Vals) ->
    gen_server:call(Pid, {write, Key, Vals}).

write_bulk_indices(Pid, MrListS) ->
    gen_server:call(Pid, {write_bulk, MrListS}, infinity).    

delete_index(Pid, Key, Vals) ->
    gen_server:call(Pid, {delete, Key, Vals}).

-record(env,
        {ets, 
         cont, 
         dbnam, 
         idx, 
         nextCP,
         chkp, 
         stop=false}).

init(DbName) ->
    Tab = indexer_trigrams:open(),
    DbIndexName = list_to_binary(DbName ++ "-idx"),
   
    case indexer_couchdb_crawler:db_exists(DbIndexName) of
        true -> ok;
        false ->
            Cont = 
                indexer_couchdb_crawler:start(
                  list_to_binary(DbName),[{reset, DbIndexName}]),
	    Check = {DbIndexName, Cont},
	    ?LOG(?INFO, "creating checkpoint:~p~n", [Check]),
	    indexer_checkpoint:init(DbIndexName, Check)
    end,
    
    {Next, {_, Cont1}} = indexer_checkpoint:resume(DbIndexName),
    ?LOG(?INFO, "resuming checkpoint: ~p ~p~n",[Next, Cont1]),
    
    {ok, #env{ets=Tab,
                      dbnam=list_to_binary(DbName),
                      idx=DbIndexName,
                      cont=Cont1,
                      nextCP=Next,
                      chkp=Cont1}}.

handle_call(ets_table, _From, S) ->
    {reply, S#env.ets, S};
handle_call(next_docs, _From, S) ->
    Cont = S#env.cont,
    case indexer_couchdb_crawler:next(Cont) of
	{docs, Docs, ContToCkP} ->
	    {reply, {ok, Docs}, S#env{chkp=ContToCkP}};
	done ->
            indexer_couchdb_crawler:compact_index(S#env.idx),
	    {reply, done, S}
    end;
handle_call(changes, _From, S) ->
    LastSeq = indexer_couchdb_crawler:read_last_seq(S#env.idx),
    {reply, indexer_couchdb_crawler:get_changes_since(S#env.dbnam, LastSeq), S};
handle_call(total_docs, _From, S) ->
    {reply, indexer_couchdb_crawler:read_doc_count(S#env.idx), S};
handle_call(db_name, _From, S) ->
    {reply, S#env.dbnam, S};
handle_call(checkpoint, _From, S) ->
    Next = S#env.nextCP,
    DbIndexName = S#env.idx,
    Next1 = indexer_checkpoint:checkpoint(Next, {DbIndexName, S#env.chkp}),
    S1 = S#env{nextCP = Next1, cont=S#env.chkp},
    {reply, ok, S1};
handle_call({checkpoint, changes, LastSeq}, _From, S) ->
    indexer_couchdb_crawler:write_last_seq(S#env.idx, LastSeq),
    {reply, ok, S};    
handle_call(schedule_stop, _From, S) ->
    ?LOG(?DEBUG, "value of checkpoint is ~p ~n",[S#env.chkp]),
    case S#env.chkp of
       {_, done} -> {reply, norun, S#env{stop=true}};
        _ -> {reply, ack, S#env{stop=true}}
    end;
handle_call({search, Str}, _From,S) ->
    Result = indexer_misc:search(Str, S#env.ets, S#env.dbnam, S#env.idx),
    {reply, Result, S};

handle_call({write, Key, Vals}, _From,S) ->
    Result = indexer_couchdb_crawler:write_indices(Key, Vals, S#env.idx),
    {reply, Result, S};
handle_call({write_bulk, MrListS}, _From,S) ->
    Result = indexer_couchdb_crawler:write_bulk(MrListS, S#env.idx),
    {reply, Result, S};
handle_call({delete, Key, Vals}, _From,S) ->
    Result = indexer_couchdb_crawler:delete_indices(Key, Vals, S#env.idx),
    {reply, Result, S};


handle_call(should_i_stop, _From, S) ->
    {reply, S#env.stop, S}.

handle_cast(stop, S) ->
    {stop, normal, S}.

terminate(Reason, S) ->
    Ets = S#env.ets,
    indexer_trigrams:close(Ets),
    ?LOG(?INFO, "stopping ~p~n",[Reason]).



    



    


