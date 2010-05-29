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

-include("indexer.hrl").

-record(state, {dbs}).

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
    {ok, #state{dbs=ets:new(names_pids,[set])}}.

db_deleted(DbName) ->
    gen_server:call(?MODULE,{db_deleted, DbName}, infinity).

db_updated(DbName) ->
    gen_server:call(?MODULE,{db_updated, DbName}, infinity).     

start_indexing(DbName) ->
    gen_server:call(?MODULE,{start, DbName}, infinity).    

search(DbName, Str) ->
    Docs = gen_server:call(?MODULE, {search, DbName, Str}, infinity),
    case Docs of
        none ->
             [];
        tooMany -> [];
        _ -> Docs
    end.

search(DbName, Str, Field) ->
    Docs = gen_server:call(?MODULE, {search, DbName, Str, Field}, infinity),
    case Docs of
        none ->
             [];
        tooMany -> [];
        _ -> Docs
    end.

stop_indexing(DbName) ->
    ?LOG(?INFO, "Scheduling a stop~n", []),
    gen_server:call(?MODULE, {stop, DbName}).

handle_call({start, DbName}, _From, State) ->

    #state{dbs=Tab} = State,
    Pid = case ets:lookup(Tab,DbName) of
              [{DbName, Pid1}] ->
                  ?LOG(?DEBUG,"An indexer for ~p already exists.~n",[DbName]),
                  Pid1;
              _Wtf ->
                  {ok, Pid2} = gen_server:start_link(indexer_server, [DbName], []),
                  ets:insert(Tab,{DbName,Pid2}),
                  Pid2
          end,

    AlreadyRunning = indexer_server:is_running(Pid),

    case AlreadyRunning of
        true ->
            {reply, ok, State};
        _ ->
            indexer_server:start(Pid),
            spawn_link(
              fun() -> 
                      couch_task_status:add_task(<<"Indexing Database">>, 
                                                 DbName, <<"Starting">>),
                      worker(Pid, 0),
                      couch_task_status:update("Complete")
              end),
            {reply, ok, State}
    end;

handle_call({search, DbName, Str}, _From, State) ->
    #state{dbs=Tab} = State,
    Pid = case ets:lookup(Tab,DbName) of
              [] ->
                  ?LOG(?DEBUG, "Indexer doesn't exist need to create new ~p ~n",[DbName]),
                  {ok, NewPid} = gen_server:start_link(indexer_server, [DbName], []),
                  ets:insert(Tab,{DbName,NewPid}),
                  NewPid;
              [{DbName,Pid2}] -> Pid2
          end,
    {reply, indexer_server:search(Pid, Str), State};

handle_call({search, DbName, Str, Field}, _From, State) ->
    #state{dbs=Tab} = State,
    Pid = case ets:lookup(Tab,DbName) of
              [] ->
                  ?LOG(?DEBUG, "Indexer doesn't exist need to create new ~p ~n",[DbName]),
                  {ok, NewPid} = gen_server:start_link(indexer_server, [DbName], []),
                  ets:insert(Tab,{DbName,NewPid}),
                  NewPid;
              [{DbName,Pid2}] -> Pid2
          end,
    {reply, indexer_server:search(Pid, Str, Field), State};

handle_call({stop, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    [{DbName,Pid}] = ets:lookup(Tab,DbName),
    ets:delete(Tab,DbName),
    {reply, indexer_server:schedule_stop(Pid), State};

handle_call({db_updated, DbName}, _From, State) ->
    ?LOG(?DEBUG,"Database: ~p ~n with state: ~p ~n , updated~n",[DbName, State]),
    #state{dbs=Tab} = State,
    ?LOG(?DEBUG,"The table is here ~p ~n",[Tab]),
    case ets:lookup(Tab,DbName) of
        [{DbName, Pid}] ->
            case indexer_server:is_running(Pid) of
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
    #state{dbs=Tab} = State,
    case ets:lookup(Tab,DbName) of
        [{DbName, Pid}] ->
            ?LOG(?DEBUG,"The Db Pid is here: ~p ~n",[Pid]),
            indexer_server:stop(Pid),
            ets:delete(Tab,DbName);
        Wtf ->
            ?LOG(?DEBUG, "There is nothing to delete for ~p ? ~n",[Wtf])
    end,
    indexer_server:delete_db_index(DbName),
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

worker(Pid, WorkSoFar) ->
    case possibly_stop(Pid) of
        void -> 
            ?LOG(?INFO, "retrieving next batch ~n",[]),
            Tbeg = now(),
            case indexer_server:next_docs(Pid) of
                {ok, Docs} ->  
                    Tind1 = now(),
                    index_these_docs(Pid, Docs),
                    Tdiff1 = timer:now_diff(now(),Tind1),
                    ?LOG(?INFO, "time spent indexing was ~p ~n",[Tdiff1]),
                    indexer_server:checkpoint(Pid),
                    ?LOG(?INFO, "indexed another ~w ~n", [length(Docs)]),
                    TotalDocs = indexer_server:total_docs(Pid),
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
                true -> indexer_server:checkpoint(Pid,changes,LastSeq),
                        couch_task_status:update(
                          "Indexed another ~p documents",
                          [length(Deletes) + length(Inserts)]);
                false -> ok
            end
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
    indexer_misc:mapreduce(F1, F2, 0, Docs).

index_these_docs(Pid, Docs) ->
    Ets = indexer_server:ets_table(Pid),
    F1 = fun(Pid1, Doc) -> indexer_words:do_indexing(Pid1, Doc, Ets) end,
    
    F2 = fun(Key, Val, Acc) ->
                 [{Key, Val} | Acc] end,
    MrList = indexer_misc:mapreduce(F1, F2, [], Docs),
    MrListS = lists:sort(fun(A, B) ->
                                 element(1,A) < element(1, B) end,
                         MrList),
    Tbeg = now(),
    indexer_server:write_bulk_indices(Pid, MrListS),
    
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


