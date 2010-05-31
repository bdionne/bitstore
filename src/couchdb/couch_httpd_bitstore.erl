-module(couch_httpd_bitstore).
-include("couch_db.hrl").

-export([handle_index_req/2, db_req/2]).

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,start_json_response/3,
    send_chunk/2,last_chunk/1,end_json_response/1,
    start_chunked_response/3, absolute_uri/2, send/2,
    start_response_length/4]).

%% -record(doc_query_args, {
%%     options = [],
%%     rev = nil,
%%     open_revs = [],
%%     update_type = interactive_edit,
%%     atts_since = nil
%% }).



db_req(#httpd{method='GET',path_parts=[_,<<"_index_query">>]}=Req, Db) ->
    Word = couch_httpd:qs_value(Req, "word","foo"),
    SlotName = couch_httpd:qs_value(Req, "field"),
    Docs = case SlotName of
               undefined ->
                   indexer:search(?b2l(Db#db.name), Word);
               _ -> 
                   indexer:search(?b2l(Db#db.name), Word, SlotName)
           end,
    send_json(Req, 200, {[
            {total_rows, length(Docs)},
            {offset, 0},
            {rows, Docs}
        ]});

db_req(#httpd{method='GET',path_parts=[_,<<"_onty">>]}=Req, Db) ->
    Subj = couch_httpd:qs_value(Req, "subj"),
    Pred = couch_httpd:qs_value(Req, "pred"),
    Obj = couch_httpd:qs_value(Req, "obj"),
    Roots = couch_httpd:qs_value(Req,"roots"),
    DbName = ?b2l(Db#db.name),

    
    Docs = case Roots of
               undefined ->
                   case Subj of
                       undefined ->
                           case Pred of
                               undefined -> bitstore:get_sources(?l2b(Obj),DbName);
                               _ -> bitstore:get_labeled_sources(?l2b(Obj),?l2b(Pred),DbName)
                           end;
                       _ ->
                           case Pred of
                               undefined -> bitstore:get_targets(?l2b(Subj),DbName);
                               _ -> bitstore:get_labeled_targets(?l2b(Subj),?l2b(Pred),DbName)
                           end               
                   end;
               _ -> bitstore:get_roots(?l2b(Pred), DbName)
           end,
    
    send_json(Req, 200, {[
            {total_rows, length(Docs)},
            {offset, 0},
            {rows, Docs}
        ]});

db_req(#httpd{method='POST',path_parts=[_,<<"_onty">>]}=Req, Db) ->
    Save = ?l2b(couch_httpd:qs_value(Req, "save")),
   
    case Save of
        undefined -> ok;
        _ -> bitstore:persist_dag(?b2l(Db#db.name))
    end,
    send_json(Req, 202, {[{ok, true}]});

db_req(#httpd{method=Method,path_parts=[_,<<"_onty">>]}=Req, Db) ->
    Subj = ?l2b(couch_httpd:qs_value(Req, "subj")),
    Pred = ?l2b(couch_httpd:qs_value(Req, "pred")),
    Obj = ?l2b(couch_httpd:qs_value(Req, "obj")),
    case Method of
        'PUT' -> bitstore:add_labeled_edge(Subj,Pred,Obj,?b2l(Db#db.name));
        'DELETE' -> bitstore:remove_labeled_edge(Subj,Pred,Obj,?b2l(Db#db.name))
    end,
    send_json(Req, 202, {[{ok, true}]}).





%% bitstore hacks
handle_index_req(#httpd{method='POST'}=Req, Db) ->
    Stop = couch_httpd:qs_value(Req, "stop","false"),
    case Stop of
	"true" -> 
	    indexer:stop_indexing(?b2l(Db#db.name));
	_ -> indexer:start_indexing(?b2l(Db#db.name))
    end,
    send_json(Req, 202, {[{ok, true}]}).
