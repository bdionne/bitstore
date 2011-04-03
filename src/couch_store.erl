% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_store).
-author('dionne@dionne-associates.com').

-export([open_db/1,
         db_info/1,
         open_doc/2,
         open_doc_db/2]).

-include("couch_db.hrl").
-include("bitstore.hrl").

-define(ADMIN_USER_CTX, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).


%% these functions are slightly modified versions of functions from Hovercraft

open_db(DbName) ->
    couch_db:open(DbName, [?ADMIN_USER_CTX]).

%%--------------------------------------------------------------------
%% Function: db_info(DbName) -> {ok,Db} | {error,Error}
%% Description: Gets the db_info as a proplist
%%--------------------------------------------------------------------
db_info(DbName) ->
    {ok, Db} = open_db(DbName),
    try
        couch_db:get_db_info(Db)
    after
        catch couch_db:close(Db)
    end.


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

open_doc_db(Db, DocId) ->
    try
        CouchDoc = couch_httpd_db:couch_doc_open(Db, DocId, nil, []),
        Doc = couch_doc:to_json_obj(CouchDoc, []),
        {ok, Doc}
    catch
        _:_Error -> ?LOG(?DEBUG,"Blew up with ~p ~n",[_Error]),
                    not_found
    end.

