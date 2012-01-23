-module(couch_events_server).
-behavior(gen_server).

-export([start_link/0]).
-export([subscribe/1, subscribe/2, unsubscribe/1, unsubscribe/2]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


-record(st, {
    channels=[]
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


subscribe(Channel) ->
    subscribe(Channel, self()).


subscribe(Channel, Client) ->
    case get_pid(Channel) of
        undefined ->
            {ok, C, R} = gen_server:call(?MODULE, {create, Channel, Client}),
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


init(_) ->
    process_flag(trap_exit, true),
    {ok, #st{}}.


terminate(_Reason, State) ->
    lists:foreach(fun({_, Pid}) ->
        exit(Pid, kill)
    end, State#st.channels),
    ok.


handle_call({create, Channel, Client}, _From, State) when is_pid(Client) ->
    {ok, Pid, Ref} = couch_events_channel:start_link(Client),
    ok = put_pid(Channel, Pid),
    Channels = [{Pid, Channel} | State#st.channels],
    {reply, {ok, Pid, Ref}, State#st{channels=Channels}};
handle_call(Mesg, _From, State) ->
    {stop, {unknown_call, Mesg}, {unknown_call, Mesg}, State}.


handle_cast(Mesg, State) ->
    {stop, {unknown_cast, Mesg}, State}.


handle_info({'EXIT', Pid, _Reason}, State) ->
    {Pid, Channel} = lists:keyfind(Pid, 2, State#st.channels),
    Channels = lists:keydelete(Pid, 1, State#st.channels),
    ok = del_pid(Channel),
    {noreply, State#st{channels=Channels}};
handle_info(Mesg, State) ->
    {stop, {unknown_info, Mesg}, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


get_pid(Key) ->
    case couch_events_kv:get(Key) of
        undefined -> undefined;
        Pid -> list_to_pid(Pid)
    end.


put_pid(Key, Pid) when is_pid(Pid) ->
    couch_events_kv:put(Key, pid_to_list(Pid)).


del_pid(Key) ->
    couch_events_kv:delete(Key).

