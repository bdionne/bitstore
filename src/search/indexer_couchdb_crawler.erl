%%%-------------------------------------------------------------------
%%% File    : indexer_couchdb_crawler.erl
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
-module(indexer_couchdb_crawler).
%%
%%
-export([start/2, 
         next/1,
         open_doc_db/2,
         open_db/1,
         index_exists/1,
         open_index/1,
         close_index/1,
         store_chkp/3,
         read_last_seq/1,
         read_doc_count/1,
         write_last_seq/2,
         get_changes_since/2,
         get_previous_version/2,
         lookup_doc_bitcask/2,
         lookup_indices/2, 
         write_indices/3,
         write_bulk/2,
         delete_db_index/1,
         delete_indices/3
        ]).

-include("../../../couchdb/src/couchdb/couch_db.hrl").
-include("indexer.hrl").

-define(BATCH_SIZE, 1000).
-define(ADMIN_USER_CTX, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).

start(DbName, [{reset, DbIndexName}]) ->
    os:cmd("rm -rf " ++ DbIndexName),
    {ok, #db{update_seq=LastSeq}} = open_db(DbName),
    {ok, DbInfo} = db_info(DbName),
    DocCount = proplists:get_value(doc_count,DbInfo),
    Db = open_index(DbIndexName),
    store_stats(Db, LastSeq, DocCount),
    [Db, {DbName, 0}].

index_exists(DbName) ->
    filelib:is_dir(DbName).

delete_db_index(DbName) ->
    DbIndexName = (DbName ++ "-idx"),
    case filelib:is_dir(DbIndexName) of
        true ->
            ?LOG(?DEBUG, "removing index for ~p ~n",[DbIndexName]),
            os:cmd("rm -rf " ++ DbIndexName);
        _ -> 
            ?LOG(?DEBUG, "index to remove does not exist ~p ~n",[DbIndexName]),
            ok
    end.

open_index(DbIndexName) ->
    bitcask:open(DbIndexName, [read_write, {max_file_size, 100000000}]).

close_index(Db) ->
    ?LOG(?DEBUG, "closing db: ~w~n",[Db]),
    bitcask:close(Db).    

next({DbName, StartId}) ->
    Docs = case StartId of
               0 -> get_all_docs(DbName, []);
               done -> [];
               _ -> get_all_docs(DbName, [{start_key, StartId}])
           end,           
    case Docs of
        [] -> done;
        {Cont, Docs1} -> {docs, Docs1, {DbName, Cont}}
    end.

open_by_id_btree(DbName) ->
    {ok, #db{fd=Fd}} = open_db(DbName),
    {ok, Header} = couch_file:read_header(Fd),
    {ok, IdBtree} = 
        couch_btree:open(Header#db_header.fulldocinfo_by_id_btree_state, Fd,
                         []),
    IdBtree.   

get_changes_since(DbName, SeqNum) ->   
    {ok, #db{update_seq=LastSeq}=Db} = open_db(DbName),
    ?LOG(?DEBUG,"In the db the lat seq is ~p ~n",[LastSeq]),
    {ok, DocInfos} = 
        couch_db:changes_since(Db, all_docs, SeqNum,
                               fun(DocInfos, Acc) ->
                                       {ok, lists:append(Acc, [DocInfos])} end,
                               [],[]),    
    {InsIds, UpdIds, DelIds} = 
        lists:foldl(fun(DocInfo, 
                        {Inserts,
                         Updates,
                         Deletes}) ->
                            ?LOG(?DEBUG, "A new Doc looks like ~p ~n",[DocInfo]),
                            {doc_info, Id, _, [{rev_info,{Rev,_},_,Deleted,_}]}=DocInfo,
                            case Rev of
                                1 -> {[Id | Inserts],Updates, Deletes};
                                _ -> case Deleted of
                                         true -> {Inserts, Updates, [Id | Deletes]};
                                         _ -> {Inserts, [Id | Updates], Deletes}
                                     end
                            end
                    end,{[],[],[]},DocInfos),
    PrevVersDocs = 
        get_previous_version(UpdIds, DbName),
    {lists:append(
       get_deleted_docs(DelIds, DbName), PrevVersDocs), 
     get_docs(lists:append(InsIds, UpdIds), DbName), LastSeq}.

get_previous_version(Ids, DbName) ->
    lists:map(
      fun(Id) ->
              ?LOG(?DEBUG,"trying to get doc: ~p ~n",[Id]),
              {ok, Db} = open_db(DbName),
              try
              DocWithRevs =
                  couch_doc:to_json_obj(couch_httpd_db:couch_doc_open(
                                          Db, Id, nil, [revs]),[revs]),
              Revs = proplists:get_value(<<"_revisions">>,element(1,DocWithRevs)),
              ?LOG(?DEBUG,"Here are the Revs: ~p ~n",[Revs]),
              LastRev = proplists:get_value(<<"start">>,element(1,Revs)),
              
                  
              PrevRevId = 
                  integer_to_list(LastRev - 1) ++ "-" ++ 
                  binary_to_list(lists:nth(2,
                                           proplists:get_value(<<"ids">>,
                                                               element(1,Revs)))),
              ?LOG(?DEBUG,"trying to get previous doc: ~p ~n",[PrevRevId]),
              couch_doc:to_json_obj(
                couch_httpd_db:couch_doc_open(Db,Id,couch_doc:parse_rev(PrevRevId),
                                              []),[])
              catch
                  _:_Error -> 
                      ?LOG(?DEBUG,"someone failed with ~p ~n",[_Error]),
                      not_found
              after
                  catch couch_db:close(Db)
              end
      end,Ids).    

get_deleted_docs(_DocIds, _DbName) ->
    [].

get_docs(DocIdList, DbName) ->
    lists:map(
      fun(Id) -> 
              {ok, Doc} = lookup_doc(Id, DbName),
              Doc
      end,
      DocIdList).    

get_all_docs(DbName, Options) ->
    IdBtree = open_by_id_btree(DbName),
    {ok, Db} = open_db(DbName),
    try
    {ok, _, Result} = 
        couch_btree:foldl(IdBtree,
                          fun(Key, Acc) ->
                                 case element(1, Acc) of
                                      0 -> {stop, Acc};
                                      _ -> TryDoc = open_doc_db(Db, element(1,Key)),
                                           case TryDoc of
                                               {ok, Doc} ->
                                                   {ok, {element(1, Acc) - 1,
                                                         [Doc | element(2, Acc)]}};
                                               _ -> {ok, {element(1, Acc), element(2, Acc)}}
                                           end
                                  end
                          end, {?BATCH_SIZE + 1,[]}, Options),
    Docs = element(2,Result),
    Bool = length(Docs) < ?BATCH_SIZE + 1,
    case length(Docs) of
        0 -> [];
        _ -> case Bool of
                 true -> {done, Docs};
                 _ -> {proplists:get_value(<<"_id">>,
                                           element(1,hd(Docs))), lists:reverse(tl(Docs))}
             end 
    end
    after
        couch_db:close(Db)
    end.

lookup_doc(Id, DbName) ->
    try        
        open_doc(DbName, Id)
    catch
        _:_ -> not_found
    end.

lookup_doc_bitcask(Id, Db) ->
    try
        {ok, Val} = bitcask:get(Db,Id),
        {ok, binary_to_term(Val)}
    catch
        _:_ -> not_found
    end.    

store_chkp(DocId, B, Db) ->
    NewDoc = case lookup_doc_bitcask(DocId, Db) of
                 {ok, Doc} ->
                     Props = element(1, Doc),
                     NewProps = proplists:delete(<<"chkp">>, Props),
                     {lists:append(NewProps, 
                                   [{<<"chkp">>,B}] )};
                 not_found -> 

                     {[{<<"_id">>, DocId},
                                {<<"chkp">>, B}]}
             end,
    store_in_cask(Db,DocId,NewDoc).

store_in_cask(Db,Key,Val) ->
    bitcask:put(Db,Key,term_to_binary(Val)).    

write_last_seq(Db, LastSeq) ->   
    NewDoc =
        case lookup_doc_bitcask(<<"db_stats">>, Db) of
            {ok, Doc} ->
                Props = element(1, Doc),
                NewProps = proplists:delete(<<"last_seq">>, Props),
                {lists:append(NewProps, 
                              [{<<"last_seq">>,LastSeq}] )};
            not_found ->
                {[{<<"_id">>, <<"db_stats">>},
                  {<<"last_seq">>, LastSeq}]}
        end,
    store_in_cask(Db,<<"db_stats">>,NewDoc).

store_stats(Db, LastSeq, DocCount) ->
    NewDoc = 
        {[{<<"_id">>, <<"db_stats">>},
          {<<"last_seq">>, LastSeq},
          {<<"doc_count">>, DocCount}]},
    store_in_cask(Db,<<"db_stats">>,NewDoc).

read_last_seq(Db) ->
    {ok, Doc} = lookup_doc_bitcask(<<"db_stats">>, Db),
    proplists:get_value(<<"last_seq">>,element(1,Doc)).

read_doc_count(Db) ->
    {ok, Doc} = lookup_doc_bitcask(<<"db_stats">>, Db),
    proplists:get_value(<<"doc_count">>,element(1,Doc)).

lookup_indices(Word, Db) ->
    case lookup_doc_bitcask(list_to_binary(Word), Db) of
        {ok, Doc} -> proplists:get_value(<<"indices">>,element(1, Doc));
        not_found -> []
    end.
 
write_bulk(MrListS, Db) ->
    lists:map(fun({Key, Vals}) ->
                      Doc = prep_doc(Key, Vals, Db),
                      store_in_cask(Db,list_to_binary(Key),Doc)
              end, MrListS).    
  
write_indices(Word, Vals, Db) ->
    BinWord = list_to_binary(Word),
    NewDoc = case lookup_doc_bitcask(BinWord, Db) of
                 {ok, Doc} -> 
                     Props = element(1, Doc),
                     Indices = proplists:get_value(<<"indices">>, Props),
                     NewProps = proplists:delete(<<"indices">>, Props),
                     {lists:append(NewProps, 
                                   [{<<"indices">>,lists:append(Indices, Vals)}] )};
                 not_found -> 
                     {[{<<"_id">>, BinWord},
                       {<<"indices">>,Vals}]}
             end,
    store_in_cask(Db,BinWord,NewDoc).

prep_doc(Word, Vals, Db) ->
   case lookup_doc_bitcask(list_to_binary(Word), Db) of
        {ok, Doc} -> 
            Props = element(1, Doc),
            Indices = proplists:get_value(<<"indices">>, Props),
            NewProps = proplists:delete(<<"indices">>, Props),
            {lists:append(NewProps, 
                              [{<<"indices">>,lists:append(Indices, Vals)}] )};
        not_found -> 
            {[{<<"_id">>, list_to_binary(Word)},
                       {<<"indices">>,Vals}]}
    end.    

delete_indices(Word, Vals, Db) ->
    case lookup_doc_bitcask(list_to_binary(Word), Db) of
        {ok, Doc} -> 
            Props = element(1, Doc),
            Indices = proplists:get_value(<<"indices">>, Props),
            NewIndices = lists:foldl(fun(Elem, Acc) ->
                                             lists:delete(Elem, Acc)
                                     end,Indices,Vals),
            NewProps = proplists:delete(<<"indices">>, Props),
            NewDoc = 
                {lists:append(NewProps, 
                              [{<<"indices">>,NewIndices}])},
            store_in_cask(Db,list_to_binary(Word),NewDoc);
        not_found -> ok
    end.
    
    
%% functions from Hovercraft  

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

    
    
