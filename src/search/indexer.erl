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
%% this is changed considerably form the book. It's been made into a gen_server
%% that maintains a hash of indexer_servers, one for each couchdb being indexed
%% or queried. The indexers work in batch as well as incrmental mode, requiring
%% two types of map/reduce functions
-author('Bob Dionne').

-export([start_link/0, stop/1, start/1, search/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(lists, [map/2]).

-behavior(gen_server).

-include("indexer.hrl").

-record(state, {dbs}).

start_link() ->
    %%osmos:start(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->    
    {ok, #state{dbs=ets:new(names_pids,[set])}}.

start(DbName) ->
    gen_server:call(?MODULE,{start, DbName}).    

search(DbName, Str) ->
    Docs = gen_server:call(?MODULE, {search, DbName, Str}, infinity),
    case Docs of
        none ->
             [];
        tooMany -> [];
        _ -> Docs
    end.

stop(DbName) ->
    ?LOG(?INFO, "Scheduling a stop~n", []),
    gen_server:call(?MODULE, {stop, DbName}).

handle_call({start, DbName}, _From, State) ->
    {ok, Pid} = gen_server:start_link(indexer_server, [DbName], []),
    #state{dbs=Tab} = State,
    ets:insert(Tab,{DbName,Pid}),
    spawn_link(
      fun() -> 
              couch_task_status:add_task(<<"Indexing Database">>, DbName, <<"Starting">>),
              %%couch_task_status:set_update_frequency(500),
              worker(Pid, 0),
              couch_task_status:update("Complete")
      end),
    {reply, ok, State};
handle_call({search, DbName, Str}, _From, State) ->
    #state{dbs=Tab} = State,
    [{DbName,Pid}] = ets:lookup(Tab,DbName),
    {reply, indexer_server:search(Pid, Str), State};
handle_call({stop, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    [{DbName,Pid}] = ets:lookup(Tab,DbName),
    {reply, indexer_server:schedule_stop(Pid), State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
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
                    ?LOG(?DEBUG, "time spent indexing was ~p ~n",[Tdiff1]),
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
            end,
            sleep(60000),
            poll_for_changes(Pid)
    end.
            
                
possibly_stop(Pid) ->
    case indexer_server:should_i_stop(Pid) of
	true ->
	    ?LOG(?INFO, "Stopping~n", []),
	    done;
    	false ->
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
    %%sleep(5000),
    
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

sleep(T) ->
    receive
    after T -> true
    end.


