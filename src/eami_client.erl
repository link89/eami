-module(eami_client).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(SOCKET_OPTS, [list, {active, once}, {packet, raw}, {reuseaddr, true}]).
-define(RECV_TIMEOUT, 5000).


%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/7]).

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
                ,username :: string() | undefined
                ,password :: string() | undefined
                ,events :: on | off

                ,reconnect_sleep :: integer() | undefined
                ,connect_timeout :: integer() | undefined
                ,socket :: port() | undefined

                ,self :: pid()
                ,seq :: integer()
               }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(Host :: string()
                 ,Port :: integer()
                 ,Username :: string()
                 ,Password :: string()
                 ,Events :: on | off
                 ,ReconnectSleep :: integer() | undefined
                 ,ConnectTimeout :: integer() | undefined) ->
    {ok, Pid::pid()} | {error, Reason::term()}.
start_link(Host, Port, Username, Events, Password, ReconnectSleep, ConnectTimeout) ->
    gen_server:start_link(?MODULE, [Host, Port, Username, Password, Events,
                                    ReconnectSleep, ConnectTimeout], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Host, Port, Username, Password, Events, ReconnectSleep, ConnectTimeout]) ->
    State = #state{host = Host
                   ,port = Port
                   ,username = Username
                   ,password = Password
                   ,events = Events

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
    LoginAction = eami_util:encode_message([{"Action", "Login"}
                                            ,{"Username", Username}
                                            ,{"Secret", Password}
                                            ,{"Events", Events}]),
    do_sync_command(Socket, LoginAction).

do_sync_command(Socket, Command) ->
    ok = inet:setopts(Socket, [{active, false}]),
    case gen_tcp:send(Socket, Command) of
        ok ->
            case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
                {ok, Raw} ->
                    Event = eami_util:decode_message(Raw),
                    case eami_util:get_value("Response", Event) of
                        "Success" ->
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
