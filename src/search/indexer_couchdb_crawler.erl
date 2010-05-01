%%%-------------------------------------------------------------------
%%% File    : indexer_couchdb_crawler.erl
%%% Author  : Robert Dionne
%%% Description :  hovercraft based crawler over couchdb databases
%%%
%%% Created :  11 Nov 2009 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% Copyright (C) 2009   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(indexer_couchdb_crawler).
%%
%%
-export([start/2, 
         next/1,
         db_exists/1, 
         store_chkp/3,
         read_last_seq/1,
         read_doc_count/1,
         write_last_seq/2,
         get_changes_since/2,
         get_previous_version/2,
         get_deleted_docs/2,
         lookup_doc/2,
         compact_index/1,
         lookup_indices/2, 
         write_indices/3,
         write_bulk/2,
         delete_indices/3
        ]).

-include("../../../couchdb/src/couchdb/couch_db.hrl").
-include("indexer.hrl").

-define(BATCH_SIZE, 1000).

start(DbName, [{reset, DbIndexName}]) ->
    hovercraft:delete_db(DbIndexName),
    hovercraft:create_db(DbIndexName), 
    {ok, #db{update_seq=LastSeq}} = hovercraft:open_db(DbName),
    {ok, DbInfo} = hovercraft:db_info(DbName),
    DocCount = proplists:get_value(doc_count,DbInfo),
    store_stats(DbIndexName, LastSeq, DocCount),
    {DbName, 0}.

db_exists(DbName) ->
    case hovercraft:open_db(DbName) of
        {ok, _} ->
            true;
        _ -> false
    end.

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
    {ok, #db{fd=Fd}} = hovercraft:open_db(DbName),
    {ok, Header} = couch_file:read_header(Fd),
    {ok, IdBtree} = 
        couch_btree:open(Header#db_header.fulldocinfo_by_id_btree_state, Fd,
                         []),
    IdBtree.   

get_changes_since(DbName, SeqNum) ->   
    {ok, #db{update_seq=LastSeq}=Db} = hovercraft:open_db(DbName),
    {ok, DocInfos} = 
        couch_db:changes_since(Db, all_docs, SeqNum,
                               fun(DocInfos, Acc) ->
                                       {ok, lists:append(Acc, DocInfos)} end,
                               [],[]),
    {InsIds, UpdIds, DelIds} = 
        lists:foldl(fun(DocInfo, 
                        {Inserts,
                         Updates,
                         Deletes}) ->
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
              {ok, Db} = hovercraft:open_db(DbName),
              DocWithRevs =
                  couch_doc:to_json_obj(couch_httpd_db:couch_doc_open(
                                          Db, Id, nil, [revs]),[revs]),
              Revs = proplists:get_value(<<"_revisions">>,element(1,DocWithRevs)),
              PrevRevId = 
                  "1-" ++ 
                  binary_to_list(lists:nth(2,
                                           proplists:get_value(<<"ids">>,
                                                               element(1,Revs)))),
              couch_doc:to_json_obj(
                couch_httpd_db:couch_doc_open(Db,Id,couch_doc:parse_rev(PrevRevId),
                                              []),[])
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
    {ok, _, Result} = 
        couch_btree:foldl(IdBtree,
                          fun(Key, Acc) ->
                                 case element(1, Acc) of
                                      0 -> {stop, Acc};
                                      _ -> TryDoc = lookup_doc(element(1,Key), DbName),
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
    end.

lookup_doc(Id, DbName) ->
    try
        hovercraft:open_doc(DbName, Id)
    catch
        _:_ -> not_found
    end.

compact_index(DbName) ->    
    {ok, Db} = hovercraft:open_db(DbName),
    couch_db:start_compact(Db).
    

store_chkp(DocId, B, DbName) ->
    case lookup_doc(DocId, DbName) of
        {ok, Doc} ->
            Props = element(1, Doc),
            NewProps = proplists:delete(<<"chkp">>, Props),
            NewDoc = 
                {lists:append(NewProps, 
                              [{<<"chkp">>,B}] )},
            hovercraft:save_doc(DbName, NewDoc);
        not_found -> 
            
            NewDoc = {[{<<"_id">>, DocId},
                       {<<"chkp">>, B}]},
            hovercraft:save_doc(DbName, NewDoc)
    end.
    

write_last_seq(DbName, LastSeq) ->
    NewDoc =
        case lookup_doc(<<"db_stats">>, DbName) of
            {ok, Doc} ->
                Props = element(1, Doc),
                NewProps = proplists:delete(<<"last_seq">>, Props),
                {lists:append(NewProps, 
                              [{<<"last_seq">>,LastSeq}] )};
            not_found ->
                {[{<<"_id">>, <<"db_stats">>},
                  {<<"last_seq">>, LastSeq}]}
        end,
    hovercraft:save_doc(DbName, NewDoc).

store_stats(DbName, LastSeq, DocCount) ->
    NewDoc = 
        {[{<<"_id">>, <<"db_stats">>},
          {<<"last_seq">>, LastSeq},
          {<<"doc_count">>, DocCount}]},
    hovercraft:save_doc(DbName, NewDoc).    

read_last_seq(DbName) ->
    {ok, Doc} = lookup_doc(<<"db_stats">>, DbName),
    proplists:get_value(<<"last_seq">>,element(1,Doc)).

read_doc_count(DbName) ->
    {ok, Doc} = lookup_doc(<<"db_stats">>, DbName),
    proplists:get_value(<<"doc_count">>,element(1,Doc)).

lookup_indices(Word, DbName) ->
    case lookup_doc(list_to_binary(Word), DbName) of
        {ok, Doc} -> proplists:get_value(<<"indices">>,element(1, Doc));
        not_found -> []
    end.
 
write_bulk(MrListS, DbName) ->
    Docs = lists:map(fun({Key, Vals}) ->
                             prep_doc(Key, Vals, DbName)
                     end,
                     MrListS),
    hovercraft:save_bulk(DbName, Docs).  
    
  
write_indices(Word, Vals, DbName) ->
    case lookup_doc(list_to_binary(Word), DbName) of
        {ok, Doc} -> 
            Props = element(1, Doc),
            Indices = proplists:get_value(<<"indices">>, Props),
            NewProps = proplists:delete(<<"indices">>, Props),
            NewDoc = 
                {lists:append(NewProps, 
                              [{<<"indices">>,lists:append(Indices, Vals)}] )},
            hovercraft:save_doc(DbName, NewDoc);
        not_found -> 
            NewDoc = {[{<<"_id">>, list_to_binary(Word)},
                       {<<"indices">>,Vals}]},
            hovercraft:save_doc(DbName, NewDoc)
    end.

prep_doc(Word, Vals, DbName) ->
   case lookup_doc(list_to_binary(Word), DbName) of
        {ok, Doc} -> 
            Props = element(1, Doc),
            Indices = proplists:get_value(<<"indices">>, Props),
            %%?LOG(?DEBUG,"the current indices ~p ~n",[Indices]),
            NewProps = proplists:delete(<<"indices">>, Props),
            %%?LOG(?DEBUG,"props after deleting ~p ~n",[NewProps]),
            {lists:append(NewProps, 
                              [{<<"indices">>,lists:append(Indices, Vals)}] )};
        not_found -> 
            {[{<<"_id">>, list_to_binary(Word)},
                       {<<"indices">>,Vals}]}
    end.
    

delete_indices(Word, Vals, DbName) ->
    case lookup_doc(list_to_binary(Word), DbName) of
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
            hovercraft:save_doc(DbName, NewDoc);
        not_found -> ok
    end.
    
    
    
              





