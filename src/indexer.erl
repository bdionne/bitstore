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

-export([start_link/0,
	 stop_indexing/1,
	 start_indexing/1,
	 search/2, search/3,
	 get_schema/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(lists, [map/2]).
-import(indexer_server, [worker/3, poll_for_changes/2]).

-behavior(gen_server).

-include("bitstore.hrl").

-record(state, {dbs, auto_index, fti_pollint}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% interested in delete/create of database events
    couch_db_update_notifier:start_link(
      fun({Event, DbName}) ->
	      gen_server:call(?MODULE,{db_event, Event, binary_to_list(DbName)}, infinity)
      end),
    %% if true, indexer automatically starts indexing new databses
    IndexDbs = list_to_atom(couch_config:get("couchdb", "fti_dbs", ?FTI_DBS)),
    FtiPollInt = list_to_integer(couch_config:get("couchdb","fti_poll_interval",?POLL_INTERVAL)),
    {ok, #state{dbs=ets:new(names_pids,[set]),
                auto_index=IndexDbs,
                fti_pollint=FtiPollInt}}.

start_indexing(DbName) ->
    gen_server:call(?MODULE,{start, DbName}, infinity).

stop_indexing(DbName) ->
    ?LOG(?INFO, "Scheduling a stop~n", []),
    gen_server:call(?MODULE, {stop, DbName}, infinity).

search(DbName, Str) ->
    check_docs(gen_server:call(?MODULE, {search, DbName, Str, all}, infinity)).

search(DbName, Str, Field) ->
    check_docs(gen_server:call(?MODULE, {search, DbName, Str, Field}, infinity)).

get_schema(DbName) ->
    gen_server:call(?MODULE,{get_schema, DbName}, infinity).

handle_call({start, DbName}, _From, State) ->
    #state{dbs=Tab, fti_pollint=PollInt} = State,
    Pid = find_or_create_idx_server(Tab, DbName, true),
    batch_index(Pid, DbName, PollInt),
    {reply, ok, State};

handle_call({stop, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    case find_or_create_idx_server(Tab, DbName, false) of
    not_found ->
        {reply, ok, State};
    Pid ->
        ets:delete(Tab,DbName),
        {reply, indexer_server:schedule_stop(Pid), State}
    end;

handle_call({search, DbName, Str, Field}, _From, State) ->
    #state{dbs=Tab} = State,
    case find_or_create_idx_server(Tab, DbName, false) of
    not_found ->
        {reply, none, State};
    Pid ->
        {reply, gen_server:call(Pid, {search, Str, Field}, infinity), State}
    end;

handle_call({get_schema, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    case find_or_create_idx_server(Tab, DbName, false) of
    not_found ->
        {reply, [], State};
    Pid ->
        {reply, gen_server:call(Pid, {get_schema}, infinity), State}
    end;


handle_call({db_event, Event, DbName}, _From, State) ->
    case Event of
	deleted ->
	    ?LOG(?DEBUG,"Database: ~p , deleted ~n",[DbName]),
	    #state{dbs=Tab} = State,
	    %% may create one even if it doesn't exist, just to make sure the index is
	    %% deleted
	    case find_or_create_idx_server(Tab,DbName, false) of
		not_found ->
		    ?LOG(?DEBUG, "We should never execut this branch, WTF! ~n",[]);
		Pid ->
		    ?LOG(?DEBUG,"The Db Pid is here: ~p ~n",[Pid]),
		    indexer_server:delete_db_index(Pid),
		    indexer_server:stop(Pid),
		    ets:delete(Tab,DbName)
	    end;
	created ->
	    ?LOG(?DEBUG,"Database: ~p ~n with state: ~p ~n , created~n",[DbName, State]),
	    #state{dbs=Tab, auto_index=Bool, fti_pollint=PollInt} = State,
	    case find_or_create_idx_server(Tab,DbName,Bool) of
		not_found ->
		    ?LOG(?DEBUG,"No index created?????????? ~n",[]),
		    ok;
		Pid ->
		    batch_index(Pid, DbName, PollInt)
	    end;
	_ -> ok
    end,
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
find_or_create_idx_server(EtsTab, DbName, CreateIfNotFound) ->
    ?LOG(?DEBUG,"Looking up indexer:  ~p ~p ~n",[DbName, CreateIfNotFound]),
    case ets:lookup(EtsTab,DbName) of
	[] ->
	    IndexName = binary_to_list(list_to_binary(DbName ++ "-idx")),
	    DbIndexName = couch_config:get("couchdb", "database_dir", ".") ++ "/bitstore/fti/" ++ IndexName,
	    case indexer_couchdb_crawler:index_exists(DbIndexName)
		orelse CreateIfNotFound of
		true ->
		    ?LOG(?DEBUG, "Index exists but isn't opened or need to create new ~p ~n",[DbName]),
		    {ok, NewPid} = gen_server:start_link(indexer_server, [DbName], [{timeout, infinity}]),
		    ets:insert(EtsTab,{DbName,NewPid}),
		    NewPid;
		_ ->
		    not_found
	    end;
	[{DbName,Pid2}] -> Pid2
    end.

check_docs(Docs) ->
    case Docs of
        none ->
             [];
        tooMany -> [];
        _ -> Docs
    end.

batch_index(Pid, DbName, PollInt) ->
    case indexer_server:is_running(Pid) of
        true ->
	    ok;
        _ ->
	    indexer_server:start(Pid),
            spawn_link(
              fun() ->
		      couch_task_status:add_task(<<"Indexing Database">>,
                                                 DbName, <<"Starting">>),
                      worker(Pid, 0, PollInt),
                      couch_task_status:update("Complete")
              end)
    end.






