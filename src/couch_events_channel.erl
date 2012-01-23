-module(couch_events_channel).
-behavior(gen_server).

-export([start_link/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


-record(st, {
    clients=[]
}).


start_link(Client) when is_pid(Client) ->
    proc_lib:start_link(?MODULE, init, [Client]).


init(Client) when is_pid(Client) ->
    erlang:monitor(process, Client),
    Ref = erlang:make_ref(),
    proc_lib:init_ack({ok, self(), Ref}),
    gen_server:enter_loop(?MODULE, [], #st{clients=[{Ref, Client}]}).


terminate(_Reason, _State) ->
    ok.


handle_call({subscribe, Client}, _From, State) when is_pid(Client) ->
    erlang:monitor(process, Client),
    Ref = erlang:make_ref(),
    Clients = [{Ref, Client} | State#st.clients],
    {reply, {ok, Ref}, State#st{clients=Clients}};
handle_call({unsubscribe, Client}, _From, State) when is_pid(Client) ->
    Clients = lists:keydelete(Client, 2, State#st.clients),
    case Clients of
        [] -> {stop, normal, ok, State};
        _ -> {reply, ok, State}
    end;
handle_call(Mesg, _From, State) ->
    {stop, {unknown_call, Mesg}, {unknown_call, Mesg}, State}.


handle_cast({notify, Event}, State) ->
    lists:foreach(fun({R, C}) ->
        C ! {event, R, Event}
    end, State#st.clients),
    {noreply, State};
handle_cast(Mesg, State) ->
    {stop, {unknown_cast, Mesg}, State}.


handle_info({'DOWN', _, process, Pid, _}, State) ->
    Clients = lists:keydelete(Pid, 2, State#st.clients),
    case Clients of
        [] -> {stop, normal, State};
        _ -> {noreply, State#st{clients=Clients}}
    end;
handle_info(Mesg, State) ->
    {stop, {unknown_info, Mesg}, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

