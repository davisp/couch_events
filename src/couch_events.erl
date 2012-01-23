-module(couch_events).


-export([start/0]).
-export([subscribe/1, subscribe/2, unsubscribe/1, unsubscribe/2]).
-export([notify/2]).


start() ->
    application:start(?MODULE).


subscribe(Channel) ->
    subscribe(Channel, self()).


subscribe(Channel, Client) ->
    case get_pid(Channel) of
        undefined ->
            {ok, C, R} = gen_server:call(couch_events_server, {create, Channel, Client}),
            link(C),
            {ok, R};
        Pid when is_pid(Pid) ->
            link(Pid),
            gen_server:call(Pid, {subscribe, Client})
    end.


unsubscribe(Channel) ->
    unsubscribe(Channel, self()).


unsubscribe(Channel, Client) ->
    case get_pid(Channel) of
        undefined ->
            throw(unknown_channel);
        Pid when is_pid(Pid) ->
            unlink(Pid),
            gen_server:call(Pid, {unsubscribe, Client})
    end.


notify(Channel, Event) ->
    % There's also a ?SEND_EVENT macro that can be used
    % to inline this code snippet.
    case get_pid(Channel) of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, {notify, Event})
    end.


get_pid(Channel) ->
    case couch_events_kv:get(Channel) of
        undefined -> undefined;
        Pid -> list_to_pid(Pid)
    end.
