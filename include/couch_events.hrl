
-define(SEND_EVENT(Channel, Event), case couch_events_kv:get(Channel) of
    Pid when is_pid(Pid) ->
        gen_server:cast(Pid, {notify, Event});
    _ ->
        ok
end).
