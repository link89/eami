-module(eami_sup).

-behaviour(supervisor).

-include("eami.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% Supervisor callbacks
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% Helper macro for declaring children of supervisor
%% ------------------------------------------------------------------

-define(GET_CFG(K, D), application:get_env(?APP_NAME, K, D)).

-define(POOL(Name, Size, Overflow, Events, OnEvent),
        {Name, {poolboy, start_link,
                [[{name, {local, Name}}
                  ,{worker_module, eami_client}
                  ,{size, Size}
                  ,{max_overflow, Overflow}
                  ,{strategy, fifo}
                 ],
                 [{host, ?GET_CFG(host, "127.0.0.1")}
                  ,{port, ?GET_CFG(port, 5038)}
                  ,{username, ?GET_CFG(username, "admin")}
                  ,{password, ?GET_CFG(password, "12345")}
                  ,{events, Events}
                  ,{on_event, OnEvent}
                  ,{reconnect_sleep, ?GET_CFG(reconnect_sleep, 1000)}
                  ,{connect_timeout, ?GET_CFG(connect_timeout, 5000)}
                 ]]},
         permanent, 5000, worker, [poolboy]}).

-define(ACTION_POOL, ?POOL(action_pool
                           ,?GET_CFG(action_pool_size, 5)
                           ,?GET_CFG(action_pool_overflow, 5)
                           ,off
                           ,fun eami_ctrl:handle_responses/2)).

-define(EVENTS_POOL, ?POOL(events_pool
                           ,?GET_CFG(events_pool_size, 2)
                           ,0
                           ,on
                           ,fun eami_ctrl:handle_events/2)).

-define(CHILDREN, [?ACTION_POOL
                   ,?EVENTS_POOL
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, ?CHILDREN}}.
