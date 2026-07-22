-module(migraterl_listener).
-moduledoc """
Opt-in reactive layer over Postgres `LISTEN`/`NOTIFY`.

Holds a single dedicated connection that `LISTEN`s on the migraterl
events channel and fans each notification out to subscribed processes.
Because `NOTIFY` fires on commit and the emitting run writes the journal
row in the same transaction, the journal is the durable record of truth
and each event is only a hint that something changed.

Subscribers receive `{migraterl_event, Channel, Payload}`, where
`Channel` and `Payload` are binaries.
""".
-behaviour(gen_server).

-export([start_link/1, subscribe/1, subscribe/2, unsubscribe/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(CHANNEL, "migraterl_events").

-record(state, {
    conn :: epgsql:connection(),
    subscribers = #{} :: #{pid() => reference()}
}).

-doc "Start a listener with the given epgsql connect config map.".
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(ConnConfig) ->
    gen_server:start_link(?MODULE, ConnConfig, []).

-doc "Subscribe the calling process to migration events.".
-spec subscribe(pid()) -> ok.
subscribe(Listener) -> subscribe(Listener, self()).

-spec subscribe(pid(), pid()) -> ok.
subscribe(Listener, Pid) -> gen_server:call(Listener, {subscribe, Pid}).

-spec unsubscribe(pid(), pid()) -> ok.
unsubscribe(Listener, Pid) -> gen_server:call(Listener, {unsubscribe, Pid}).

init(ConnConfig) ->
    process_flag(trap_exit, true),
    case epgsql:connect(ConnConfig) of
        {ok, Conn} ->
            %% Cold-start catch-up is the host application's job (read the
            %% journal); here we just start receiving hints.
            case epgsql:squery(Conn, "LISTEN " ++ ?CHANNEL) of
                {ok, _, _} -> {ok, #state{conn = Conn}};
                Other -> {stop, {listen_failed, Other}}
            end;
        {error, Reason} ->
            {stop, {connect_failed, Reason}}
    end.

handle_call({subscribe, Pid}, _From, #state{subscribers = Subs} = State) ->
    Ref = erlang:monitor(process, Pid),
    {reply, ok, State#state{subscribers = Subs#{Pid => Ref}}};
handle_call({unsubscribe, Pid}, _From, #state{subscribers = Subs} = State) ->
    {reply, ok, State#state{subscribers = demonitor_sub(Pid, Subs)}};
handle_call(_Req, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% epgsql delivers async notifications to the connection-owning process.
handle_info({epgsql, Conn, {notification, Channel, _Pid, Payload}}, #state{conn = Conn} = State) ->
    Event = {migraterl_event, Channel, Payload},
    _ = [P ! Event || P <- maps:keys(State#state.subscribers)],
    {noreply, State};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #state{subscribers = Subs} = State) ->
    {noreply, State#state{subscribers = maps:remove(Pid, Subs)}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    _ = epgsql:close(Conn),
    ok.

demonitor_sub(Pid, Subs) ->
    case Subs of
        #{Pid := Ref} ->
            erlang:demonitor(Ref, [flush]),
            maps:remove(Pid, Subs);
        _ ->
            Subs
    end.
