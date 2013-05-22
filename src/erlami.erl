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

-export( [
    get_response/0, parse_event/1
]).

parse_response(Response) when is_record(Response, ami_response)  ->
    receive
        { tcp, _Socket, <<"Message: ", Message/binary>> } ->
            parse_response(Response#ami_response{ message = erlami_helpers:trim(Message) });
        { tcp, _Socket, <<"\r\n">> } ->
            Response;
        Msg ->
            #ami_response{ success = false, message = io_lib:format("Unexpected message ~p", [Msg]) }
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
            io:format("Unexpected message: ~p\n", [Msg]),
            fail
    end.

%% External API

get_response() ->
    receive
        { tcp, _Socket, <<"Response: ", "Success", "\r\n">> } ->
            parse_response(#ami_response{ success = true });
        Msg ->
            #ami_response{ success = false, message = io_lib:format("Unexpected message ~p", [Msg]) }
    end.
