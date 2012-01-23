-module(couch_events_server).
-behavior(gen_server).

-export([start_link/0]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


-record(st, {
    channels=[]
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    process_flag(trap_exit, true),
    {ok, #st{}}.


terminate(_Reason, State) ->
    lists:foreach(fun({Pid, _}) ->
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
    case lists:keyfind(Pid, 1, State#st.channels) of
        {value, {Pid, Channel}} ->
            Channels = lists:keydelete(Pid, 1, State#st.channels),
            ok = del_pid(Channel),
            {noreply, State#st{channels=Channels}};
        false ->
            % Something in the code reloading stuff spawns processes
            % that trigger this clause. Ignore them and hope for
            % the best.
            {noreply, State}
    end;
handle_info(Mesg, State) ->
    {stop, {unknown_info, Mesg}, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


put_pid(Key, Pid) when is_pid(Pid) ->
    couch_events_kv:put(Key, pid_to_list(Pid)).


del_pid(Key) ->
    couch_events_kv:delete(Key).

