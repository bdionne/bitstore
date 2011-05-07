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

-export([worker/3,
	 poll_for_changes/2,
         get_changes/1,
	 ets_table/1,
	 checkpoint/1,
         checkpoint/3,
	 schedule_stop/1,
         write_index/3,
         write_bulk_indices/2,
	 delete_index/3,
         delete_db_index/1,
         is_running/1,
         start/1,
	 stop/1,
         stop_scheduled/1]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-import(filename, [join/2]).
-include("bitstore.hrl").

schedule_stop(Pid) ->
    gen_server:call(Pid, schedule_stop, infinity).

stop_scheduled(Pid) ->
    gen_server:call(Pid, scheduled_stop, infinity).

is_running(Pid) ->
    gen_server:call(Pid, is_running, infinity).

start(Pid) ->
    gen_server:call(Pid, start, infinity).

stop(Pid) ->
     gen_server:cast(Pid, stop).

%% db_name(Pid) ->
%%     gen_server:call(Pid, db_name).

get_changes(Pid) ->
     gen_server:call(Pid, changes, infinity).

checkpoint(Pid) -> gen_server:call(Pid, checkpoint).
checkpoint(Pid, changes, LastSeq) ->
    gen_server:call(Pid, {checkpoint, changes, LastSeq}).

ets_table(Pid)  -> gen_server:call(Pid, ets_table).

write_index(Pid, Key, Vals) ->
    gen_server:call(Pid, {write, Key, Vals}).

write_bulk_indices(Pid, MrListS) ->
    gen_server:call(Pid, {write_bulk, MrListS}, infinity).

delete_index(Pid, Key, Vals) ->
    gen_server:call(Pid, {delete, Key, Vals}).

delete_db_index(Pid) ->
    gen_server:call(Pid, {delete_db_index}, infinity).

-record(env,
        {ets,
         cont,
         dbnam,
	 idx_name,
         idx,
         nextCP,
         chkp,
         running=false,
         sched_stop=false}).

init(DbName) ->
    Tab = indexer_trigrams:open(),
    IndexName = binary_to_list(list_to_binary(DbName ++ "-idx")),
    DbIndexName = couch_config:get("couchdb", "database_dir", ".") ++ "/bitstore/fti/" ++ IndexName,

    Db = case indexer_couchdb_crawler:index_exists(DbIndexName) of
         true -> indexer_couchdb_crawler:open_index(DbIndexName);
         false ->
             ?LOG(?DEBUG,"Starting new crawler with ~p ~p ~n",[list_to_binary(DbName), DbIndexName]),
             [Db1, Cont] = indexer_couchdb_crawler:start(list_to_binary(DbName),[{reset, DbIndexName}]),
             ?LOG(?INFO, "creating checkpoint:~p~n", [Cont]),
             indexer_checkpoint:init(Db1, Cont),
             Db1
         end,

    {Next, Cont1} = indexer_checkpoint:resume(Db),
    ?LOG(?INFO, "resuming checkpoint: ~p ~p~n",[Next, Cont1]),

    {ok, #env{ets=Tab,
                      dbnam=list_to_binary(DbName),
	              idx_name=DbIndexName,
                      idx=Db,
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
            %% bitcask presumably doesn't need compaction??
            %%indexer_couchdb_crawler:compact_index(S#env.idx),
	    {reply, done, S#env{running=false}}
    end;
handle_call(changes, _From, S) ->
    LastSeq = indexer_couchdb_crawler:read_last_seq(S#env.idx),
    ?LOG(?DEBUG,"The last seqeuence was ~p ~n",[LastSeq]),
    {reply, indexer_couchdb_crawler:get_changes_since(S#env.dbnam, LastSeq), S};
handle_call(total_docs, _From, S) ->
    {reply, indexer_couchdb_crawler:read_doc_count(S#env.idx), S};
%% handle_call(db_name, _From, S) ->
%%     {reply, S#env.dbnam, S};
handle_call(checkpoint, _From, S) ->
    Next = S#env.nextCP,
    Next1 = indexer_checkpoint:checkpoint(Next, S#env.chkp),
    S1 = S#env{nextCP = Next1, cont=S#env.chkp},
    {reply, ok, S1};
handle_call({checkpoint, changes, LastSeq}, _From, S) ->
    indexer_couchdb_crawler:write_last_seq(S#env.idx, LastSeq),
    {reply, ok, S};
handle_call(schedule_stop, _From, S) ->
    ?LOG(?DEBUG, "value of checkpoint is ~p ~n",[S#env.chkp]),
    case S#env.chkp of
    {_, done} -> {reply, norun, S#env{running=false, sched_stop=true}};
    _ -> {reply, ack, S#env{sched_stop=true}}
    end;
handle_call(start, _From, S) ->
    {reply, ok, S#env{running=true}};
handle_call(is_running, _From, S) ->
    {reply, S#env.running, S};
handle_call(scheduled_stop, _From, S) ->
    {reply, S#env.sched_stop, S};
handle_call({search, Str, Field}, _From,S) ->
    Result = indexer_misc:search(Str, Field, S#env.ets, S#env.dbnam, S#env.idx),
    {reply, Result, S};
handle_call({get_schema}, _From,S) ->
    Result = indexer_couchdb_crawler:get_schema(S#env.idx),
    {reply, Result, S};

handle_call({write, Key, Vals}, _From,S) ->
    Result = indexer_couchdb_crawler:write_indices(Key, Vals, S#env.idx),
    {reply, Result, S};
handle_call({write_bulk, MrListS}, _From,S) ->
    Result = indexer_couchdb_crawler:write_bulk(MrListS, S#env.idx),
    {reply, Result, S};
handle_call({write_schema_slots, SlotNames}, _From, S) ->
    Result = indexer_couchdb_crawler:write_schema_slots(SlotNames, S#env.idx),
    {reply, Result, S};
handle_call({delete_db_index}, _From,S) ->
    Result = indexer_couchdb_crawler:delete_db_index(S#env.idx_name),
    {reply, Result, S};
handle_call({index_exists, DbName}, _From,S) ->
    IndexName = binary_to_list(list_to_binary(DbName ++ "-idx")),
    DbIndexName = couch_config:get("couchdb", "database_dir", ".") ++ "/fti/" ++ IndexName,
    {reply, indexer_couchdb_crawler:index_exists(DbIndexName), S};
handle_call({delete, Key, Vals}, _From,S) ->
    Result = indexer_couchdb_crawler:delete_indices(Key, Vals, S#env.idx),
    {reply, Result, S}.

handle_cast(stop, S) ->
    {stop, normal, S}.

terminate(Reason, S) ->
    indexer_couchdb_crawler:close_index(S#env.idx),
    Ets = S#env.ets,
    indexer_trigrams:close(Ets),
    ?LOG(?INFO, "stopping ~p~n",[Reason]).

worker(Pid, WorkSoFar, PollInt) ->
    case possibly_stop(Pid) of
    void ->
        ?LOG(?INFO, "retrieving next batch ~n",[]),
        Tbeg = now(),
        case gen_server:call(Pid, next_docs, infinity) of
        {ok, Docs} ->
            Tind1 = now(),
            index_these_docs(Pid, Docs),
            Tdiff1 = timer:now_diff(now(),Tind1),
            ?LOG(?INFO, "time spent indexing was ~p ~n",[Tdiff1]),
            indexer_server:checkpoint(Pid),
            ?LOG(?INFO, "indexed another ~w ~n", [length(Docs)]),
            TotalDocs = gen_server:call(Pid, total_docs),
            WorkSoFarNew = WorkSoFar + length(Docs),
            couch_task_status:update("Indexed ~p of ~p changes (~p%)",
                                     [WorkSoFarNew, TotalDocs, (WorkSoFarNew*100) div TotalDocs]),
            case possibly_stop(Pid) of
            done -> ok;
            void ->
                Totdiff = timer:now_diff(now(),Tbeg),
                ?LOG(?DEBUG, "time spent total was ~p ~n",[Totdiff]),
                ?LOG(?DEBUG, "percentage spent in indexing was ~p ~n",
                     [Tdiff1 / Totdiff ]),
                worker(Pid, WorkSoFarNew, PollInt)
            end;
        done ->
            %% we now go into polling mode
            %% and start polling for new updates to the db
            couch_task_status:update
              ("batch indexing complete, monitoring for changes"),
            poll_for_changes(Pid, PollInt)
        end
    end.

poll_for_changes(Pid, PollInt) ->
    case possibly_stop(Pid) of
        done ->
             ok;
        void ->
            {Deletes, Inserts, LastSeq} = indexer_server:get_changes(Pid),
            %% first do the deletes BECAUSE they contain previous revisions
            %% of docs for the updated case. When a doc has been added we simplying
            %% updating the index by just doing a delete followed by an insertion
            %% for the new version of the doc
            index_these_docs(Pid,Deletes,false),
            ?LOG(?INFO, "indexed another ~w ~n", [length(Deletes)]),
            % then do the inserts
            index_these_docs(Pid,Inserts,true),
            ?LOG(?INFO, "indexed another ~w ~n", [length(Inserts)]),
            %% then updates
            %% checkpoint only if there were changes
            case length(Deletes) > 0 orelse length(Inserts) > 0 of
                true ->
		    indexer_server:checkpoint(Pid,changes,LastSeq),
		    couch_task_status:update(
		      "Indexed another ~p documents",
		      [length(Deletes) + length(Inserts)]);
                false ->
		    ok
            end,
	    %% sleep(PollInt),
            timer:sleep(PollInt),
	    poll_for_changes(Pid, PollInt)
    end.


possibly_stop(Pid) ->
    case indexer_server:stop_scheduled(Pid) of
        true ->
            ?LOG(?INFO, "Stopping~n", []),
            indexer_server:stop(Pid),
            done;
        _ ->
            void
    end.

index_these_docs(Pid, Docs, InsertOrDelete) ->
    Ets = indexer_server:ets_table(Pid),
    F1 = fun(Pid1, Doc) -> indexer_words:do_indexing(Pid1, Doc, Ets) end,
    F2 = fun(Key, Val, Acc) -> handle_result(Pid, Key, Val, Acc, InsertOrDelete) end,
    {_, SlotNames} = indexer_misc:mapreduce(F1, F2, 0, Docs),
    gen_server:call(Pid, {write_schema_slots, SlotNames}, infinity).


index_these_docs(Pid, Docs) ->
    Ets = indexer_server:ets_table(Pid),
    F1 = fun(Pid1, Doc) -> indexer_words:do_indexing(Pid1, Doc, Ets) end,

    F2 = fun(Key, Val, Acc) ->
                 [{Key, Val} | Acc] end,
    {MrList, SlotNames} = indexer_misc:mapreduce(F1, F2, [], Docs),
    ?LOG(?DEBUG, "The slot names are: ~p ~n",[SlotNames]),
    MrListS = lists:sort(fun(A, B) ->
				 element(1,A) < element(1, B) end,
                         MrList),
    Tbeg = now(),
    indexer_server:write_bulk_indices(Pid, MrListS),

    gen_server:call(Pid, {write_schema_slots, SlotNames}, infinity),

    Tdiff = timer:now_diff(now(),Tbeg),
    ?LOG(?DEBUG, "time spent in writing was ~p ~n",[Tdiff]).


handle_result(Pid, Key, Vals, Acc, InsertOrDelete) ->
    case InsertOrDelete of
        true ->
            indexer_server:write_index(Pid, Key, Vals);
        false ->
            indexer_server:delete_index(Pid, Key, Vals)
    end,
    Acc + 1.











