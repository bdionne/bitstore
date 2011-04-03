-module(bitstore_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


% This app does absolutely nothing, it's here to make rebar work :)

start(_StartType, _StartArgs) ->
    ok.

stop(_State) ->
    ok.
