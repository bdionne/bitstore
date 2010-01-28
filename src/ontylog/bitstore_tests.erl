%%%-------------------------------------------------------------------
%%% File    : bitstore_tests.erl
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
%%% Created :  27 Jan 2010 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% bitstore, Copyright (C) 2009-2010   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(bitstore_tests).
-author('dionne@dionne-associates.com').
%%
%%
-include_lib("eunit/include/eunit.hrl").
%%
%%
-import(load_triple_store, [load_table/1, name_to_id/2]).
%%
%%
%%
add_simple_link_test() ->
    NameTable = load_table(diamond_spec()),
    {SubId, _} = name_to_id(d, NameTable),
    {PredId, _} = name_to_id(p, NameTable),
    ?assert(length(bitstore:get_nodes(SubId,PredId,diamond)) =:= 2).
    

diamond_spec() ->
    [diamond, [{d, p, c},
           {d, p, b},
           {c, p, a},
           {b, p, a}]].


