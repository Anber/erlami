%%% Main erlami module.
%%%
%%% Copyright 2013 Anton Evzhakov <anber@anber.ru>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
-module(erlami).

-author("Anton Evzhakov <anber@anber.ru>").
-license("Apache License 2.0").

-include("erlami.hrl").

-export([init/1, start_link/1]).

-export( [
    request/1, request/2, add_handler/2
]).

-record(state, {handlers = dict:new(), responses = dict:new()}).

start_link(ServerInfo) ->
    {ok, spawn_link(?MODULE, init, [ServerInfo])}.

init(ServerInfo) ->
    AmiPID = self(),
    register(ami, AmiPID),
    {ok, _ClientPID} = erlami_client:start_link(ServerInfo, AmiPID),
    receive
        {connected, Socket} ->
            loop(#state{}, Socket);
        _ -> fail
    end.

loop(State, Socket) ->
    NewState = receive
        Event = #ami_event{action_id = ActionID} when ActionID =/= undefined ->
            State#state{
                responses = case dict:is_key(ActionID, State#state.responses) of
                    true ->
                        {Pid, true} = dict:fetch(ActionID, State#state.responses),
                        Pid ! Event,
                        dict:erase(ActionID, State#state.responses);
                    false ->
                        lager:error(<<"Ответ на неизвестный запрос: ~p~n~p">>, [Event, dict:to_list(State#state.responses) ]),
                        State#state.responses
                end
            };
        Event = #ami_event{name = EventName} ->
            State#state{
                handlers = case dict:is_key(EventName, State#state.handlers) of
                    true ->
                        Fns = dict:fetch(EventName, State#state.handlers),
                        Actualed = lists:filter(
                            fun (F) ->
                                case F(Event) of
                                    continue -> true;
                                    _ -> false
                                end
                            end,
                            Fns
                        ),
                        dict:store(EventName, Actualed, State#state.handlers);
                    false -> State#state.handlers
                end
            };
        Response = #ami_response{uuid = ActionID} ->
            State#state{
                responses = case dict:is_key(ActionID, State#state.responses) of
                    true ->
                        {Pid, IsAsync} = dict:fetch(ActionID, State#state.responses),
                        case IsAsync of
                            true ->
                                State#state.responses;
                            false ->
                                Pid ! Response,
                                dict:erase(ActionID, State#state.responses)
                        end;
                    false ->
                        lager:error(<<"Ответ на неизвестный запрос: ~p~n~p">>, [Response, dict:to_list(State#state.responses) ]),
                        State#state.responses
                end
            };
        { handler, add, EventName, F } ->
            State#state{
                handlers = dict:append(EventName, F, State#state.handlers)
            };
        { request, Req, PidToResponse } when is_record(Req, ami_request) ->
            #ami_request{action = Action, data = Data} = Req,
            {ActionID, IsAsync} = erlami_client:request(Socket, Action, Data),
            State#state{responses = dict:store(ActionID, {PidToResponse, IsAsync}, State#state.responses)};
        Msg ->
            lager:error(<<"Unexpected message: ~p~n">>, [Msg]),
            State
    end,
    loop(NewState, Socket).

%% External API

request(Request) when is_record(Request, ami_request) ->
    ami ! {request, Request, self()},
    receive
        Response when is_record(Response, ami_response) -> Response;
        Event when is_record(Event, ami_event) -> Event
    end.

request(Action, Data) ->
    request(#ami_request{action = Action, data = Data}).

%% @spec add_handler(EventName, Callback) -> ok
%% @doc …
add_handler(EventName, Callback) when is_atom(EventName), is_function(Callback, 1) ->
    ami ! { handler, add, EventName, fun(Event) -> Callback(Event#ami_event.data) end },
    ok.