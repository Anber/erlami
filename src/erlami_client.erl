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
-export([start_link/2]).

%% callbacks
-export([init/2]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(Name, ServerInfo) ->
    {ok, spawn_link(?MODULE, init, [Name, ServerInfo])}.

auth(Socket, User, Password) ->
    gen_tcp:send(Socket, "Action: login\n"),
    gen_tcp:send(Socket, io_lib:format("Username: ~s\n", [User])),
    gen_tcp:send(Socket, io_lib:format("Secret: ~s\n", [Password])),
    gen_tcp:send(Socket, "\n"),
    case erlami:get_response() of
        Response when is_record(Response, ami_response), Response#ami_response.success == true ->
            ok;
        Response when is_record(Response, ami_response) ->
            io:format("Error: ~p\n", [Response#ami_response.message]), 
            fail
    end.

init(Name, ServerInfo) ->
    {host, Host} = lists:keyfind(host, 1, ServerInfo),
    {port, Port} = lists:keyfind(port, 1, ServerInfo),
    {user, User} = lists:keyfind(user, 1, ServerInfo),
    {password, Password} = lists:keyfind(password, 1, ServerInfo),
    % {logfile, Logfile} = lists:keyfind(logfile, 1, ServerInfo),

    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, line}]),

    AuthResult = receive
        { tcp, Socket, <<"Asterisk Call Manager/1.4\r\n">> } ->
            auth(Socket, User, Password);
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            fail
    end,
    case AuthResult of
        ok ->
            register(ami, self()), 
            event_loop(dict:new());
        _ -> fail
    end.

event_loop(Handlers) ->
    NHandlers = receive
        { tcp, _Socket, <<"Event: ", Name/binary>> } ->
            EventName = binary_to_atom(erlami_helpers:trim(Name), utf8),
            Event = erlami:parse_event(#ami_event{ name = EventName }),
            case dict:is_key(EventName, Handlers) of
                true ->
                    lists:foreach(fun (F) -> F(Event) end, dict:fetch(EventName, Handlers));
                false -> nop
            end,
            Handlers;
        { handler, add, EventName, F } ->
            L = case dict:is_key(EventName, Handlers) of
                true -> dict:fetch(EventName, Handlers);
                false -> []
            end,
            dict:store(EventName, [F | L], Handlers);
        Msg ->
            io:format("Unexpected message: ~p\n", [Msg]),
            Handlers
    end,
    event_loop(NHandlers).

    % {ok, #hostent{h_addr_list=Addresses}} = resolve_host(Host),
    % Log = erlagi_log:get_logger(Logfile),
    % [Address|_] = Addresses,
    % Options = [
    %     {ip, Address},
    %     {active, false},
    %     {reuseaddr, true},
    %     {backlog, Backlog}
    % ],
    % {ok, Socket} = gen_tcp:listen(Port, Options),
    % Children = for_loop(1, Backlog, fun(IterNumber) ->
    %     WorkerName = list_to_atom(string:join([
    %         "fastagi",
    %         Host,
    %         integer_to_list(Port),
    %         integer_to_list(IterNumber)
    %         ], "-"
    %     )),
    %     ?CHILD(WorkerName, [Socket, Log, Callback])
    % end),
