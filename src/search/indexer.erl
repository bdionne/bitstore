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
-import(indexer_server, [worker/2, poll_for_changes/1]).

-behavior(gen_server).

-include("bitstore.hrl").

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

    
    case indexer_server:is_running(Pid) of
        true ->
	    ok;
        _ ->
	    indexer_server:start(Pid),
            spawn_link(
              fun() -> 
		      couch_task_status:add_task(<<"Indexing Database">>, 
                                                 DbName, <<"Starting">>),
                      
                      worker(Pid, 0),
                      couch_task_status:update("Complete")
              end)
    end,
    {reply, ok, State};

    


handle_call({search, DbName, Str}, _From, State) ->
    #state{dbs=Tab} = State,
    Pid = find_or_create_idx_server(Tab, DbName),
    {reply, gen_server:call(Pid, {search, Str}, infinity), State};

handle_call({search, DbName, Str, Field}, _From, State) ->
    #state{dbs=Tab} = State,
    Pid = find_or_create_idx_server(Tab, DbName),
    {reply, gen_server:call(Pid, {search, Str, Field}, infinity), State};

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
		    indexer_server:start(Pid),
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

%%
%% private helpers
%%
find_or_create_idx_server(EtsTab, DbName) ->
    case ets:lookup(EtsTab,DbName) of
	[] ->
	    ?LOG(?DEBUG, "Indexer doesn't exist need to create new ~p ~n",[DbName]),
	    {ok, NewPid} = gen_server:start_link(indexer_server, [DbName], []),
	    ets:insert(EtsTab,{DbName,NewPid}),
	    NewPid;
	[{DbName,Pid2}] -> Pid2
    end.
    



