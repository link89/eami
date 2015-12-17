-module(eami_restful).

-include("eami.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([send_action/1
         ,handle_ami_responses/1
         ,handle_ami_events/1]).

%% ------------------------------------------------------------------
%% AMI Action Sending API
%% ------------------------------------------------------------------

send_action(_Action) -> 'ok'.

%% ------------------------------------------------------------------
%% AMI Action Responses handler
%% ------------------------------------------------------------------

handle_ami_responses(Responses) ->
    lists:map(fun handle_ami_response/1, Responses).

handle_ami_response(_Response) -> ok.

%% ------------------------------------------------------------------
%% AMI Events handler
%% ------------------------------------------------------------------

handle_ami_events(Events) ->
    lists:map(fun handle_ami_event/1, Events).

handle_ami_event(_Event) -> ok.

