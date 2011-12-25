%%%-------------------------------------------------------------------
%%% File    : tries.erl
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
%%% bitstore, Copyright (C) 2009-2011   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(tries).
-author('dionne@dionne-associates.com').

-export([tbit/1]).

%% first pull out some interesting keys
tbit(RegExp) ->
    Db = bitcask:open("../couchdb/tmp/lib/bitstore/fti/biomedgt-idx"),
    Matches
        = lists:foldl(fun(Key, Acc) ->
                   case re:run(Key,RegExp,[{capture, none}]) of
                   match ->
                       [Key | Acc];
                   _ ->
                       Acc
                   end
                     end,[],bitcask:list_keys(Db)),
    lists:reverse(Matches).

