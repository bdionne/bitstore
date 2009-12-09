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
-module(indexer_misc).
-author('Joe Armstrong').
%%
%% minor tweaks to integrate with couchdb
-author('Bob Dionne').



-export([foreach_word_in_string/3, 
	 mapreduce/4, search/4]).
-import(lists, [filter/2, foreach/2, map/2, reverse/1]).

-include("indexer.hrl").

foreach_word_in_string(Str, F, Acc) ->
    case get_word(Str) of
	no -> 
	    Acc;
	{Word, Str1} ->
	    Acc1 = F(Word, Acc),
	    foreach_word_in_string(Str1, F, Acc1)
    end.

isWordChar(X) when $A=< X, X=<$Z -> true;
isWordChar(X) when $0=< X, X=<$9 -> true;
isWordChar(X) when $a=< X, X=<$z -> true;
isWordChar(_)  -> false.

get_word([H|T]) ->
    case isWordChar(H) of
	true  -> collect_word(T, [H]);
	false -> get_word(T)
    end;
get_word([]) ->
    no.

collect_word([H|T]=All, L) ->
    case isWordChar(H) of
	true  -> collect_word(T, [H|L]);
	false -> {reverse(L), All}
    end;
collect_word([], L) ->
    {reverse(L), []}.

mapreduce(F1, F2, Acc0, L) ->
    S = self(),
    Pid = spawn(fun() -> reduce(S, F1, F2, Acc0, L) end),    
    receive
	{Pid, Result} ->
	    Result
    end.

reduce(Parent, F1, F2, Acc0, L) ->
    process_flag(trap_exit, true),
    ReducePid = self(),
    %% Create the Map processes
    %%   One for each element X in L
    foreach(fun(X) -> 
		    spawn_link(fun() -> do_job(ReducePid, F1, X) end)
	    end, L),
    N = length(L),
    %% make a dictionary to store the Keys
    Dict0 = dict:new(),
    %% Wait for N Map processes to terminate
    Dict1 = collect_replies(N, Dict0),
    
    Acc = dict:fold(F2, Acc0, Dict1),
    
    Parent ! {self(), Acc}.

%% collect_replies(N, Dict)
%%     collect and merge {Key, Value} messages from N processes.
%%     When N processes have terminate return a dictionary
%%     of {Key, [Value]} pairs
collect_replies(0, Dict) ->
    Dict;
collect_replies(N, Dict) ->
    receive
        {'EXIT', _,  _Why} ->
	    collect_replies(N-1, Dict);
	{Key, Val} ->
            case dict:is_key(Key, Dict) of
		true ->
		    Dict1 = dict:append(Key, Val, Dict),
		    collect_replies(N, Dict1);
		false ->
		    Dict1 = dict:store(Key,[Val], Dict),
		    collect_replies(N, Dict1)
	    end
	
    end.

do_job(ReducePid, F, X) ->
    F(ReducePid, X).


search(Str, Ets, DbName, Idx) ->
    %% find the keywords using the same algorithm as in the indexing phase
    F1 = fun(Word, Acc) -> [Word|Acc] end,
    Words = indexer_misc:foreach_word_in_string(Str, F1, []),
    L1 = map(fun(I) -> indexer_words:process_word(I, Ets) end, Words),
    Words1 = [W || {yes, W} <- L1],
    Indices = 
        map(fun(I) -> 
                    indexer_couchdb_crawler:lookup_indices(I, Idx) end, Words1),
    Sets = [sets:from_list(X) || X <- Indices, X =/= []],
    case Sets of 
	[] ->
	    none;
	_ ->
	    Unique = sets:intersection(Sets),
	    Indices1 = sets:to_list(Unique),
	    case length(Indices1) of
		N when N > 200 ->
		    tooMany;
		_ -> 
                    map(fun(I) ->
                                {ok, Doc} = hovercraft:open_doc(DbName, I),
                                Doc
                        end, Indices1)
	    end
    end.

        
            
    
                  

