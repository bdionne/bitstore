%%%-------------------------------------------------------------------
%%% File    : concept.erl
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
%%% Created :  02 Jan 2010 by Robert Dionne <dionne@dionne-associates.com>
%%%
%%% bitstore, Copyright (C) 2009-2010   Dionne Associates, LLC.
%%%-------------------------------------------------------------------
-module(concept).
-author('dionne@dionne-associates.com').
%%
%% A concept is the fundamental term in ontylog. Concepts have parents and relations to
%% other concepts which make up it's logical definition. In bitstore concepts are also
%% couchdb docs, so for the most part all the inputs and outputs to these functions are 
%% docs. I've debated using just the ids instead but suspect there will be times when
%% other bits of the doc are required, .eg. whether or not a concept is primitive or
%% defined.
-export([is_primitive/1,
         defined_parents/2,
         defined_children/2,
         inferred_parents/2,
         inferred_children/2,
         defined_roles/2,
         inferred_roles/2]).
%%
%% so far it appears the information about a concept being primitive or defined is the
%% only bit that needs to be in the doc itself, but it might be prudent since ontylog
%% is a domain specific extension to couchdb to have all these function take docs as inputs
%% and produce docs on output, grabbing the ids as needed to talk to the Dag/triple-store
%% layer and use hovercraft to look up docs by id as needed
%%


