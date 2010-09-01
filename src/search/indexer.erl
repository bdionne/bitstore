%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.

%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%
%%  Original copyright: "(c) 2007 armstrongonsoftware"
%%---
-module(indexer).
-author('Joe Armstrong').
%%
%% this is changed considerably from the book. It's been made into a gen_server
%% that maintains a hash of indexer_servers, one for each couchdb being indexed
%% or queried. The indexers work in batch as well as incremental mode, requiring
%% two types of map/reduce functions
%%
%%% Copyright (C) 2009   Dionne Associates, LLC.
-author('Bob Dionne').

-export([start_link/0, stop_indexing/1, start_indexing/1, search/2, search/3, db_deleted/1, db_updated/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(lists, [map/2]).

-behavior(gen_server).

-include("bitstore.hrl").

-record(state, {}).

-record(env,
        {ets, 
         cont, 
         dbnam, 
         idx, 
         nextCP,
         chkp,
         running=false,
         sched_stop=false}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> 
    couch_db_update_notifier:start_link(
      fun({deleted, DbName}) ->
              ?MODULE:db_deleted(binary_to_list(DbName));              
         ({updated, DbName}) ->
              ?MODULE:db_updated(binary_to_list(DbName));              
         (_Else) ->
              ok
      end),
    ets:new(names_indexers,[set, public, named_table]),
    {ok, #state{}}.

db_deleted(DbName) ->
    gen_server:call(?MODULE,{db_deleted, DbName}, infinity).

db_updated(DbName) ->
    gen_server:call(?MODULE,{db_updated, DbName}, infinity).     

start_indexing(DbName) ->
    gen_server:call(?MODULE,{start, DbName}, infinity).

stop_indexing(DbName) ->
    ?LOG(?INFO, "Scheduling a stop~n", []),
    gen_server:call(?MODULE, {stop, DbName}).


check_docs(Docs) ->
    case Docs of
        none ->
             [];
        tooMany -> [];
        _ -> Docs
    end.    

search(DbName, Str) ->
    check_docs(gen_server:call(?MODULE, {search, DbName, Str}, infinity)).    

search(DbName, Str, Field) ->
    check_docs(gen_server:call(?MODULE, {search, DbName, Str, Field}, infinity)).


handle_call({start, DbName}, _From, State) ->

    Pid = find_or_create_idx_server(DbName),

    AlreadyRunning = Pid#env.running,

    case AlreadyRunning of
        true ->
            {reply, ok, State};
        _ ->
            Pid1 = Pid#env{running=true},
	    update_idx(DbName, Pid1),
            spawn_link(
              fun() -> 
                      couch_task_status:add_task(<<"Indexing Database">>, 
                                                 DbName, <<"Starting">>),
                      worker(Pid1, 0),
                      couch_task_status:update("Complete")
              end),
            {reply, ok, State}
    end;


handle_call({search, DbName, Str}, _From, State) ->
    Pid = find_or_create_idx_server(DbName),
    Result = indexer_misc:search(Str, all, Pid#env.ets, Pid#env.dbnam, Pid#env.idx),
    {reply, Result, State};

handle_call({search, DbName, Str, Field}, _From, State) ->
    Pid = find_or_create_idx_server(DbName),
    Result = indexer_misc:search(Str, Field, Pid#env.ets, Pid#env.dbnam, Pid#env.idx),
    {reply, Result, State};

handle_call({stop, DbName}, _From, State) ->
    [{DbName,Pid}] = ets:lookup(names_indexers,DbName),

    case Pid#env.chkp of
       {_, done} -> 
	    Pid1 = Pid#env{running=false, sched_stop=true},
	    update_idx(DbName,Pid1),
	    {reply, norun, State};
        _ ->
	    Pid1 = Pid#env{sched_stop=true},
	    update_idx(DbName,Pid1),
	    {reply, ack, State}
    end;

handle_call({db_updated, DbName}, _From, State) ->
    ?LOG(?DEBUG,"Database: ~p ~n with state: ~p ~n , updated~n",[DbName, State]),
    case ets:lookup(names_indexers,DbName) of
        [{DbName, Pid}] ->
            case Pid#env.running of
                false -> 
                    spawn_link(
              fun() -> 
                      couch_task_status:add_task(
                        <<"Indexing Database">>, DbName, <<"Starting">>),
                      poll_for_changes(Pid),
                      couch_task_status:update("Complete")
              end);
                _ -> ok
            end;
        Wtf ->
            ?LOG(?DEBUG,"Where is the Pid for this guy? ~p ~n",[Wtf])
    end,
    {reply, ok, State};

handle_call({db_deleted, DbName}, _From, State) ->
    ?LOG(?DEBUG,"Database: ~p , deleted ~n",[DbName]),
    case ets:lookup(names_indexers,DbName) of
        [{DbName, Pid}] ->
            ?LOG(?DEBUG,"The Db Pid is here: ~p ~n",[Pid]),
	    indexer_couchdb_crawler:close_index(Pid#env.idx),
	    Ets = Pid#env.ets,
	    indexer_trigrams:close(Ets),
	    %%
            ets:delete(names_indexers,DbName);
        Wtf ->
            ?LOG(?DEBUG, "There is nothing to delete for ~p ? ~n",[Wtf])
    end,
    indexer_couchdb_crawler:delete_db_index(DbName),
    {reply, ok, State}.
    

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("terminate called with: ~w ~n",[_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

next_docs(Pid) ->
    Cont = Pid#env.cont,
    case indexer_couchdb_crawler:next(Cont) of
	{docs, Docs, ContToCkP} ->
	    Pid1 = Pid#env{chkp=ContToCkP},
	    update_idx(binary_to_list(Pid#env.dbnam), Pid1),
	    {ok, Docs};
	    
	done ->
	    Pid1 = Pid#env{running=false},
	    update_idx(Pid#env.dbnam, Pid1),
	    done
    end.

checkpoint(Pid) ->
    Next = Pid#env.nextCP,
    Next1 = indexer_checkpoint:checkpoint(Next, Pid#env.chkp),
    Pid1 = Pid#env{nextCP = Next1, cont=Pid#env.chkp},
    update_idx(Pid#env.dbnam, Pid1),
    ok.

total_docs(Pid) ->
    indexer_couchdb_crawler:read_doc_count(Pid#env.idx).


get_changes(Pid) ->
    LastSeq = indexer_couchdb_crawler:read_last_seq(Pid#env.idx),
    ?LOG(?DEBUG,"The last seqeuence was ~p ~n",[LastSeq]),
    indexer_couchdb_crawler:get_changes_since(Pid#env.dbnam, LastSeq).

worker(Pid, WorkSoFar) ->
    case possibly_stop(Pid) of
        void -> 
            ?LOG(?INFO, "retrieving next batch ~n",[]),
            Tbeg = now(),
            case next_docs(Pid) of
                {ok, Docs} ->  
                    Tind1 = now(),
                    index_these_docs(Pid, Docs),
                    Tdiff1 = timer:now_diff(now(),Tind1),
                    ?LOG(?INFO, "time spent indexing was ~p ~n",[Tdiff1]),
                    checkpoint(Pid),
                    ?LOG(?INFO, "indexed another ~w ~n", [length(Docs)]),
                    TotalDocs = total_docs(Pid),
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
                            worker(Pid, WorkSoFarNew)
                    end;
                done ->
                    %% we now go into polling mode
                    %% and start polling for new updates to the db
                    couch_task_status:update
                      ("batch indexing complete, monitoring for changes"),
                    poll_for_changes(Pid)                    
            end
    end.

poll_for_changes(Pid) ->
    case possibly_stop(Pid) of
        done ->
             ok;
        void ->
            {Deletes, Inserts, LastSeq} = get_changes(Pid),
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
                true -> indexer_couchdb_crawler:write_last_seq(Pid#env.idx, LastSeq),
			couch_task_status:update(
                          "Indexed another ~p documents",
                          [length(Deletes) + length(Inserts)]);
                false -> ok
            end
    end.


possibly_stop(Pid) ->
    case Pid#env.sched_stop of
        true ->
            ?LOG(?INFO, "Stopping~n", []),
	    case ets:lookup(names_indexers,Pid#env.dbnam) of
		[{DbName, Pid}] ->
		    ?LOG(?DEBUG,"The Db Pid is here: ~p ~n",[Pid]),
		    indexer_couchdb_crawler:close_index(Pid#env.idx),
		    Ets = Pid#env.ets,
		    indexer_trigrams:close(Ets),
		    ets:delete(names_indexers,DbName);
		Wtf ->
		    ?LOG(?DEBUG, "There is nothing to delete for ~p ? ~n",[Wtf])
	    end,
	    indexer_couchdb_crawler:delete_db_index(Pid#env.dbnam),
	    done;
	_ ->
	    void
    end.

index_these_docs(Pid, Docs, InsertOrDelete) ->
    Ets = Pid#env.ets,
    F1 = fun(Pid1, Doc) -> indexer_words:do_indexing(Pid1, Doc, Ets) end,    
    F2 = fun(Key, Val, Acc) -> handle_result(Pid, Key, Val, Acc, InsertOrDelete) end,
    indexer_misc:mapreduce(F1, F2, 0, Docs).

index_these_docs(Pid, Docs) ->
    Ets = Pid#env.ets,
    F1 = fun(Pid1, Doc) -> indexer_words:do_indexing(Pid1, Doc, Ets) end,
    
    F2 = fun(Key, Val, Acc) ->
                 [{Key, Val} | Acc] end,
    MrList = indexer_misc:mapreduce(F1, F2, [], Docs),
    MrListS = lists:sort(fun(A, B) ->
				 element(1,A) < element(1, B) end,
                         MrList),
    Tbeg = now(),
    indexer_couchdb_crawler:write_bulk(MrListS, Pid#env.idx),
    
    Tdiff = timer:now_diff(now(),Tbeg),
    ?LOG(?DEBUG, "time spent in writing was ~p ~n",[Tdiff]).
    

handle_result(Pid, Key, Vals, Acc, InsertOrDelete) ->
    case InsertOrDelete of
        true ->
	    indexer_couchdb_crawler:write_indices(Key, Vals, Pid#env.idx);
        false ->
	    indexer_couchdb_crawler:delete_indices(Key, Vals, Pid#env.idx)
    end,    
    Acc + 1.
%%
%% private helpers
%%
find_or_create_idx_server(DbName) ->
    case ets:lookup(names_indexers,DbName) of
	[] ->
	    ?LOG(?DEBUG, "Indexer doesn't exist need to create new ~p ~n",[DbName]),
	    Tab = indexer_trigrams:open(),
	    DbIndexName = list_to_binary(DbName ++ "-idx"),

	    Db = case indexer_couchdb_crawler:index_exists(binary_to_list(DbIndexName)) of
		     true -> indexer_couchdb_crawler:open_index(binary_to_list(DbIndexName));
		     false ->
			 ?LOG(?DEBUG,"Starting new crawler with ~p ~p ~n",
			      [list_to_binary(DbName), binary_to_list(DbIndexName)]),
			 [Db1, Cont] =
			     indexer_couchdb_crawler:start(list_to_binary(DbName),
							   [{reset, binary_to_list(DbIndexName)}]),
			 ?LOG(?INFO, "creating checkpoint:~p~n", [Cont]),
			 indexer_checkpoint:init(Db1, Cont),
			 Db1
		 end,
	    {Next, Cont1} = indexer_checkpoint:resume(Db),
	    ?LOG(?INFO, "resuming checkpoint: ~p ~p~n",[Next, Cont1]),
	    ets:insert(names_indexers,{DbName,#env{ets=Tab,
                      dbnam=list_to_binary(DbName),
                      idx=Db,
                      cont=Cont1,
                      nextCP=Next,
                      chkp=Cont1}}),
	    [{_, Res}] = ets:lookup(names_indexers,DbName),
	    Res;
	[{DbName,Pid2}] -> Pid2
    end.

update_idx(DbName, NewPid) ->
    ets:delete(names_indexers,DbName),
    ets:insert(names_indexers,{DbName, NewPid}).
    



