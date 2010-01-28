%%%-------------------------------------------------------------------
%%% File    : bitstore.erl
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
%%% Created :  19 Jan 2010 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% bitstore, Copyright (C) 2009-2010   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(bitstore).
-author('dionne@dionne-associates.com').


-behaviour(gen_server).

%% API
-export([start_link/0, 
         add_triple/4,
         remove_triple/4,
         get_nodes/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(dag, [build_dag/1, 
              add_edge/2,
              get_nodes/2,
              remove_edge/2]).

-record(state, {dbs}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% add graph edge to existing graph
add_triple(SubId, PredId, ObjId, DbName) ->
    gen_server:call(?MODULE, {add_triple, {SubId, PredId, ObjId}, DbName}, infinity).

%% delete graph edge from existing graph
remove_triple(SubId, PredId, ObjId, DbName) ->
    gen_server:call(?MODULE, {remove_triple, {SubId, PredId, ObjId}, DbName}, infinity).

get_nodes(SubId, PredId, DbName) ->
    gen_server:call(?MODULE, {get_nodes, {SubId, PredId}, DbName}, infinity).
    

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{dbs=ets:new(dbnames_graphs,[set])}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_triple, Triplet, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),
    Dag2 = add_edge(Dag, Triplet),
    ets:insert(Tab,{DbName, Dag2}),              
    {reply, ok, #state{dbs=Tab}};
handle_call({remove_triple, Triplet, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),    
    Dag2 = remove_edge(Dag, Triplet),
    ets:insert(Tab,{DbName, Dag2}),              
    {reply, ok, #state{dbs=Tab}};
handle_call({get_nodes, Pair, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),
    Nodes = get_nodes(Dag, Pair),
    {reply, Nodes, State}.
    

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
find_or_build_dag(Tab, DbName) ->
    case ets:lookup(Tab, DbName) of
        [] ->
            build_dag(DbName);
        [{DbName, ExistingDag}] -> ExistingDag
    end.
