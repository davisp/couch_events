-module(couch_events_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    couch_events_sup:start_link().

stop(_State) ->
    ok.
