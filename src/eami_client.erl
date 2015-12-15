-module(eami_client).

-behaviour(gen_server).

-include("eami.hrl").

-define(SERVER, ?MODULE).

-define(SOCKET_OPTS, [binary, {active, once}, {packet, raw}, {reuseaddr, true}]).
-define(RECV_TIMEOUT, 5000).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/8]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% record for gen_server state
%% ------------------------------------------------------------------

-record(state, {host :: string() | undefined
                ,port :: integer() | undefined
                ,username :: binary() | string() | undefined
                ,password :: binary() | string() | undefined
                ,events :: on | off

                ,on_event :: fun() | undefined
                ,fragment :: binary()

                ,reconnect_sleep :: integer() | undefined
                ,connect_timeout :: integer() | undefined
                ,socket :: port() | undefined

                ,self :: pid()
                ,seq :: integer()
               }).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(Host :: string()
                 ,Port :: integer()
                 ,Username :: binary() | string()
                 ,Password :: binary() | string()
                 ,Events :: on | off
                 ,OnEvent :: fun() | undefined
                 ,ReconnectSleep :: integer() | undefined
                 ,ConnectTimeout :: integer() | undefined) ->
    {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Host, Port, Username, Events, OnEvent, Password, ReconnectSleep, ConnectTimeout) ->
    gen_server:start_link(?MODULE, [Host, Port, Username, Password, Events, OnEvent,
                                    ReconnectSleep, ConnectTimeout], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Host, Port, Username, Password, Events, OnEvent, ReconnectSleep, ConnectTimeout]) ->
    State = #state{host = Host
                   ,port = Port
                   ,username = Username
                   ,password = Password
                   ,events = Events

                   ,on_event = OnEvent
                   ,fragment = <<>>

                   ,reconnect_sleep = ReconnectSleep
                   ,connect_timeout = ConnectTimeout

                   ,self = self()
                   ,seq = 0},

    case connect(State) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {stop, {connection_error, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Receive data from socket, see handle_response/2. Match `Socket' to
%% enforce sanity.
handle_info({tcp, Socket, Bs}, #state{socket = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, handle_response(Bs, State)};

handle_info({tcp, Socket, _}, #state{socket = OurSocket} = State)
  when OurSocket =/= Socket ->
    %% Ignore tcp messages when the socket in message doesn't match
    %% our state.
    {noreply, State};

handle_info({tcp_error, _Socket, _Reason}, State) ->
    %% This will be followed by a close
    {noreply, State};

%% Socket got closed
handle_info({tcp_closed, _Socket}, #state{reconnect_sleep = no_reconnect } = State) ->
    %% If we aren't going to reconnect, then there is nothing else for
    %% this process to do.
    {stop, normal, State#state{socket = undefined}};

handle_info({tcp_closed, _Socket}, State) ->
    Self = self(),
    spawn(fun() -> reconnect_loop(Self, State) end),

    %% Throw away the socket and the queue, as we will never get a
    %% response to the requests sent on the old socket. The absence of
    %% a socket is used to signal we are "down"
    {noreply, State#state{socket = undefined}};

%% Redis is ready to accept requests, the given Socket is a socket
%% already connected and authenticated.
handle_info({connection_ready, Socket}, #state{socket = undefined} = State) ->
    {noreply, State#state{socket = Socket}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

connect(#state{host = Host
               ,port = Port
               ,username = Username
               ,password = Password
               ,events = Events
               ,connect_timeout = ConnectTimeout} = State) ->
    case gen_tcp:connect(Host, Port, ?SOCKET_OPTS, ConnectTimeout) of
        {ok, Socket} ->
            case authenticate(Socket, Username, Password, Events) of
                ok ->
                    {ok, State#state{socket = Socket}};
                {error, Reason} ->
                    {error, {authentication_error, Reason}}
            end;
        {error, Reason} ->
            {error, {connection_error, Reason}}
    end.

authenticate(Socket, Username, Password, Events) ->
    LoginAction = eami_util:encode_message([{<<"Action">>, <<"Login">>}
                                            ,{<<"Username">>, Username}
                                            ,{<<"Secret">>, Password}
                                            ,{<<"Events">>, Events}]),
    do_sync_command(Socket, LoginAction).

do_sync_command(Socket, Command) ->
    ok = inet:setopts(Socket, [{active, false}]),
    case gen_tcp:send(Socket, Command) of
        ok ->
            case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
                {ok, Data} ->
                    Event = eami_util:decode_message(Data),
                    case eami_util:get_value(<<"Response">>, Event) of
                        <<"Success">> ->
                            ok = inet:setopts(Socket, [{active, once}]);
                        Other ->
                            {error, {unexpected_data, Other}}
                    end;
                Other ->
                    {error, {unexpected_data, Other}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec handle_response(binary(),state()) -> state().
handle_response(Data, #state{on_event = OnEvent
                             ,fragment = Fragment} = State) ->
    case eami_util:parse(Data, Fragment) of
        {Messages, NewFragment} ->
            OnEvent(Messages),
            State#state{fragment = NewFragment};
        _ ->
            State
    end.

%% @doc: Loop until a connection can be established
reconnect_loop(Client, #state{reconnect_sleep = ReconnectSleep} = State) ->
    case catch(connect(State)) of
        {ok, #state{socket = Socket}} ->
            gen_tcp:controlling_process(Socket, Client),
            Client ! {connection_ready, Socket};
        {error, _Reason} ->
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State);
        _ ->
            timer:sleep(ReconnectSleep),
            reconnect_loop(Client, State)
    end.
