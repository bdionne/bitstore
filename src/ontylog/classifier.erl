%%%-------------------------------------------------------------------
%%% File    : classifier.erl
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
-module(classifier).
-author('dionne@dionne-associates.com').

-include("bitstore.hrl").

-export([classify/1,
         subsumes_p/2]).
%%
-import(bitcask, [get/2,put/3,fold/3]).
%%
%%
classify(Dag) ->
    %% create new digraph and add vertex for each node in the cask, using the key for the label
    %% add single edge for each link
    NewFacts = fold(Dag,
                    fun(_Id,Concept,Acc) ->
                            classify(Dag,Concept) ++ Acc
                    end,[]),
    store_new_facts(NewFacts).
%%
%% 0. topologically sort concepts with respect to the "isa" relation
%% 1. compute the LUBs for the concept
%% 2. using the LUBs as roots, compute the GLBs
%% 
%% If 1. and 2. result in a single concept and it's the same in both cases, then we
%% have an Eq.
%%
%% 3. remove any subsumptions between any of the GLBs and LUBs
%% 4. add any new subsumptions given in the GLBs and LUBs not already accounted for
%%    by definitions
%%
%% Normally this requires marking algorithms to make it efficient, detect diamonds, etc..
%% it's not clear how to pull this off without writing some temporary bits in the cask. Perhaps
%% the digraph and digraph_utils modules will be sufficient, as they support labels on vertices.
%% They can be used to store colors, prim/def bits, visited, and classified bits for recursive 
%% calls
%%
classify(_Dag, _Concept) ->    
    ok.
                 
%%
%%
subsumes_p(_Node1,_Node2) ->
    true.
%%
%%
store_new_facts(_NewFacts) ->
    ok.
