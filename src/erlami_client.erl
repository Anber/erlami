%%% A AMI client.
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
-module(erlami_client).
-author("Anton Evzhakov <anber@anber.ru>").
-license("Apache License 2.0").

-include_lib("kernel/include/inet.hrl").
-include_lib("erlami.hrl").

%% API
-export([start_link/2, request/3]).

%% callbacks
-export([init/2]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(ServerInfo, AmiPID) ->
    {ok, spawn_link(?MODULE, init, [ServerInfo, AmiPID])}.

parse_response(Response) when is_record(Response, ami_response)  ->
    receive
        { tcp, _Socket, <<"Message: ", Message/binary>> } ->
            parse_response(Response#ami_response{ message = erlami_helpers:trim(Message) });
        { tcp, _Socket, <<"ActionID: ", Uuid/binary>> } ->
            parse_response(Response#ami_response{ uuid = binary_to_list(erlami_helpers:trim(Uuid)) });
        { tcp, _Socket, <<"\r\n">> } ->
            Response;
        Msg ->
            #ami_response{ success = false, message = io_lib:format(<<"Unexpected message ~p">>, [Msg]) }
    end.

get_response() ->
    receive
        { tcp, _Socket, <<"Response: ", "Success", "\r\n">> } ->
            parse_response(#ami_response{ success = true });
        { tcp, _Socket, <<"Response: ", "Error", "\r\n">> } ->
            parse_response(#ami_response{ success = false });
        Msg ->
            #ami_response{ success = false, message = io_lib:format(<<"Unexpected message ~p">>, [Msg]) }
    end.


parse_event(Event) when is_record(Event, ami_event)  ->
    receive
        { tcp, _Socket, <<"Privilege: ", Privileges/binary>> } ->
            parse_event(Event#ami_event{ privilege = binary:split(erlami_helpers:trim(Privileges), <<",">>) });
        { tcp, _Socket, <<"\r\n">> } ->
            Event;
        { tcp, _Socket, Msg } when is_binary(Msg) ->
            [Key, Value] = binary:split(erlami_helpers:trim(Msg), <<": ">>),
            parse_event(Event#ami_event{ data = [{Key, Value} | Event#ami_event.data] });
        Msg ->
            lager:error(<<"Unexpected message: ~p~n">>, [Msg]),
            fail
    end.

%% @spec request(Socket, Action, Data) -> #ami_response
%% @doc посылает запрос в AMI и возвращает ответ
request(Socket, Action, Data) ->
    ActionID = uuid:to_string(uuid:uuid1()),
    gen_tcp:send(Socket, io_lib:format("Action: ~s\n", [Action])),
    gen_tcp:send(Socket, io_lib:format("ActionID: ~s\n", [ActionID])),
    lists:foreach(fun ({K, V}) ->
        gen_tcp:send(Socket, io_lib:format("~s: ~s\n", [K, V]))
    end, Data), 
    gen_tcp:send(Socket, "\n"),
    ActionID.

%% @spec request(Socket, Request) -> #ami_response
%% @doc посылает запрос в AMI и возвращает ответ
request(Socket, #ami_request{action = Action, data = Data}) ->
    request(Socket, Action, Data).

auth(Socket, User, Password) ->
    request(Socket, login, [{'Username', User}, {'Secret', Password}]),
    case get_response() of
        Response when is_record(Response, ami_response), Response#ami_response.success == true ->
            ok;
        Response when is_record(Response, ami_response) ->
            lager:error(<<"Ошибка авторизации в AMI: ~p~n">>, [Response#ami_response.message]),
            fail
    end.

init(ServerInfo, AmiPID) ->
    {host, Host} = lists:keyfind(host, 1, ServerInfo),
    {port, Port} = lists:keyfind(port, 1, ServerInfo),
    {user, User} = lists:keyfind(user, 1, ServerInfo),
    {password, Password} = lists:keyfind(password, 1, ServerInfo),
    % {logfile, Logfile} = lists:keyfind(logfile, 1, ServerInfo),

    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, line}]),

    receive
        { tcp, Socket, <<"Asterisk Call Manager/1.4\r\n">> } ->
            AmiPID ! {connected, Socket},
            auth(Socket, User, Password),
            event_loop(Socket, AmiPID);
        Msg ->
            lager:error(<<"Unexpected message: ~p~n">>, [Msg]),
            fail
    end.
    % case AuthResult of
    %     ok ->
    %     _ -> fail
    % end.

event_loop(Socket, AmiPID) ->
    receive
        { tcp, _Socket, <<"Event: ", Name/binary>> } ->
            EventName = binary_to_atom(erlami_helpers:trim(Name), utf8),
            Event = parse_event(#ami_event{ name = EventName }),
            AmiPID ! Event;
        { tcp, _Socket, <<"Response: ", "Success", "\r\n">> } ->
            Response = parse_response(#ami_response{ success = true }),
            AmiPID ! Response;
        { tcp, _Socket, <<"Response: ", "Error", "\r\n">> } ->
            Response = parse_response(#ami_response{ success = false }),
            AmiPID ! Response;
        Msg ->
            lager:error(<<"Unexpected message: ~p~n">>, [Msg])
    end,
    event_loop(Socket, AmiPID).