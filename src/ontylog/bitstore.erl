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
         add_labeled_edge/4,
         remove_labeled_edge/4,
         get_labeled_targets/3,
         get_labeled_sources/3,
         get_targets/2,
         get_sources/2,
         get_roots/2,
         is_related/4,
         persist_dag/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-import(dag, [create_or_open_dag/2, 
              add_edge/2,
              remove_edge/2,
              get_edge_targets/2,
              get_edge_sources/2,
              path_exists/2,
              close_dag/1]).

-include("../../../couchdb/src/couchdb/couch_db.hrl").
-define(ADMIN_USER_CTX, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).

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
add_labeled_edge(SubId, PredId, ObjId, DbName) ->
    gen_server:call(?MODULE, {add_triple, {SubId, PredId, ObjId}, DbName}, infinity).

%% delete graph edge from existing graph
remove_labeled_edge(SubId, PredId, ObjId, DbName) ->
    gen_server:call(?MODULE, {remove_triple, {SubId, PredId, ObjId}, DbName}, infinity).

get_labeled_targets(SubId, PredId, DbName) ->
    Ids = gen_server:call(?MODULE, {get_edge_targets, {SubId, PredId}, DbName}, infinity),
    lists:map(fun(I) ->
                      get_doc(DbName, I)
              end, Ids).

get_labeled_sources(ObjId, PredId, DbName) ->
    Ids = gen_server:call(?MODULE, {get_edge_sources, {ObjId, PredId}, DbName}, infinity),
    lists:map(fun(I) ->
                      get_doc(DbName, I)
              end, Ids).   


get_targets(SubId, DbName) ->
    Def = gen_server:call(?MODULE, {get_edges, SubId, DbName}, infinity),
    lists:map(fun({PredId,Vals}) ->
                      PredDoc = get_doc(DbName, PredId),
                      {[{"pred", PredDoc}, {"vals", 
                                            lists:map(fun(Id) ->
                                                              get_doc(DbName, Id)
                                                  end, Vals)}]}
              end, Def).

get_sources(ObjId, DbName) ->
    Def = gen_server:call(?MODULE, {get_in_edges, ObjId, DbName}, infinity),
    lists:map(fun({SubId,PredId}) ->
                      {[{"subj", get_doc(DbName, SubId)},{"pred", get_doc(DbName,PredId)}]}
              end, Def).

get_roots(Pred, DbName) ->
    Ids = gen_server:call(?MODULE, {get_roots, Pred, DbName}, infinity),
    lists:map(fun(I) ->
                      get_doc(DbName, I)
              end, Ids).                      

is_related(SubId,PredId,TargetId,DbName) ->
    gen_server:call(?MODULE, {path_exists, {SubId, PredId, TargetId}, DbName}, infinity).

persist_dag(DbName) ->
    gen_server:call(?MODULE, {persist_dag, DbName}, infinity).

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
    Dag = find_or_build_dag(State#state.dbs, DbName),
    add_edge(Triplet, Dag),
    {reply, ok, State};
handle_call({remove_triple, Triplet, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),    
    remove_edge(Triplet, Dag),
    {reply, ok, #state{dbs=Tab}};
handle_call({get_edge_targets, Pair, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),
    Nodes = get_edge_targets(Pair, Dag),
    {reply, Nodes, State};
handle_call({get_edge_sources, Pair, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),
    Nodes = get_edge_sources(Pair, Dag),
    {reply, Nodes, State};
handle_call({get_edges, SubId, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),
    ConceptDef = dag:get_targets(SubId, Dag),
    {reply, ConceptDef, State};
handle_call({get_in_edges, SubId, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),
    ConceptDef = dag:get_sources(SubId, Dag),
    {reply, ConceptDef, State};
handle_call({get_roots, PredId, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),
    Ids = dag:get_roots(PredId, Dag),
    {reply, Ids, State};
handle_call({path_exists, Triple, DbName}, _From, State) ->
    #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),
    {reply, path_exists(Triple, Dag), State};
handle_call({persist_dag, DbName}, _From, State) ->
     #state{dbs=Tab} = State,
    Dag = find_or_build_dag(Tab, DbName),
    {reply, close_dag(Dag), State}. 
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
            NewDag = create_or_open_dag(DbName, true),
            ets:insert(Tab, {DbName, NewDag}),
            NewDag;
        [{DbName, ExistingDag}] -> 
            ExistingDag
    end.

get_doc(DbName, DocId) ->
    {ok, Doc} = open_doc(list_to_binary(DbName), DocId),
    Doc.


open_db(DbName) ->
    couch_db:open(DbName, [?ADMIN_USER_CTX]).


%%--------------------------------------------------------------------
%% Function: open_doc(DbName, DocId) -> {ok,Doc} | {error,Error}
%% Description: Gets the eJSON form of the Document
%%--------------------------------------------------------------------
open_doc(DbName, DocId) ->
    {ok, Db} = open_db(DbName),
    try
        CouchDoc = couch_httpd_db:couch_doc_open(Db, DocId, nil, []),
        Doc = couch_doc:to_json_obj(CouchDoc, []),
        {ok, Doc}
    after
        catch couch_db:close(Db)
    end.
