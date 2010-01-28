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
foo_test() ->
    ?assert(true).
%%
add_simple_link_test() ->
    bitstore:add_triple(<<"01">>,<<"02">>,<<"03">>,onearc),
    ?assert(length(bitstore:get_nodes(<<"01">>,<<"02">>, onearc)) =:= 1),
    bitstore:add_triple(<<"01">>,<<"02">>,<<"04">>,onearc),
    ?assert(length(bitstore:get_nodes(<<"01">>,<<"02">>, onearc)) =:= 2),
    ?assert(hd(bitstore:get_nodes(<<"01">>,<<"02">>, onearc)) =:= <<"04">>),
    bitstore:remove_triple(<<"01">>,<<"02">>,<<"03">>,onearc),
    ?assert(length(bitstore:get_nodes(<<"01">>,<<"02">>, onearc)) =:= 1).
