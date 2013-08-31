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

-behaviour(gen_server).

-export([start_link/1, loop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

-include_lib("kernel/include/inet.hrl").
-include_lib("erlami.hrl").

-record(state, {socket, pid}).

-spec start_link(list({atom(), any()})) -> {ok, pid()}.
start_link(ServerInfo) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ServerInfo, []).

-spec init(list({atom(), any()})) -> {ok, #state{}}.
init(ServerInfo) ->
    % process_flag(trap_exit, true),

    {host, Host} = lists:keyfind(host, 1, ServerInfo),
    {port, Port} = lists:keyfind(port, 1, ServerInfo),
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, line}]),

    receive
        { tcp, Socket, <<"Asterisk Call Manager/", _Version:3/binary, "\r\n">> } ->
            {user, User} = lists:keyfind(user, 1, ServerInfo),
            {password, Password} = lists:keyfind(password, 1, ServerInfo),
            ok = auth(Socket, User, Password);
        Msg ->
            lager:error(<<"Unexpected message: ~p~n">>, [Msg]),
            erlang:error(unexpected_message)
    end,

    Pid = spawn_link(?MODULE, loop, [Socket]), 
    gen_tcp:controlling_process(Socket, Pid),
    {ok, #state{socket = Socket, pid = Pid}}.

-spec terminate(atom(), #state{}) -> ok.
terminate(_, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

-spec auth(gen_tcp:socket(), string(), string()) -> ok | fail.
auth(Socket, User, Password) ->
    Response = request(Socket, login, [{'Username', User}, {'Secret', Password}]),
    case Response of
        #ami_response{success = true} ->
            ok;
        #ami_response{message = Msg} ->
            lager:error(<<"Ошибка авторизации в AMI: ~p~n">>, [Msg]),
            fail
    end.

-spec request(gen_tcp:socket(), atom(), [{atom(), string()}]) -> #ami_response{}.
request(Socket, Action, Data) ->
    ActionID = uuid:to_string(uuid:uuid1()),
    gen_tcp:send(Socket, io_lib:format("Action: ~s\n", [Action])),
    gen_tcp:send(Socket, io_lib:format("ActionID: ~s\n", [ActionID])),
    lists:foreach(fun ({K, V}) ->
        gen_tcp:send(Socket, io_lib:format("~s: ~s\n", [K, V]))
    end, Data), 
    gen_tcp:send(Socket, "\n"),
    get_response().

loop(Socket) ->
    receive
        {request, Action, Data, {Pid, Ref}} ->
            Response = request(Socket, Action, Data),
            Pid ! {Ref, Response};
        {tcp, _Socket, <<"Event: ", Name/binary>>} ->
            EventName = binary_to_atom(erlami_helpers:trim(Name), utf8),
            Event = parse_event(#ami_event{ name = EventName }),
            erlami_evm:notify(Event); 
        {tcp_closed, _Socket} ->
            lager:error(<<"Потеряно соединение с Asterisk Manager Interface">>),
            erlang:error(tcp_closed);
        Msg ->
            lager:error(<<"Unexpected message: ~p">>, [Msg])
    after
        5000 ->
            request(Socket, ping, [])
    end,
    ?MODULE:loop(Socket).

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({request, Action, Data}, _From, State = #state{pid = Pid}) ->
    Ref = make_ref(),
    Pid ! {request, Action, Data, {self(), Ref}},
    Response = receive
        {Ref, Resp} -> Resp
    end,
    {reply, Response, State}.

parse_response(Response) when is_record(Response, ami_response)  ->
    receive
        { tcp, _Socket, <<"Message: ", Message/binary>> } ->
            parse_response(Response#ami_response{ message = erlami_helpers:trim(Message) });
        { tcp, _Socket, <<"Ping: Pong\r\n">> } ->
            parse_response(Response);
        { tcp, _Socket, <<"Timestamp: ", Timestamp/binary>> } ->
            parse_response(Response#ami_response{ timestamp = erlami_helpers:trim(Timestamp) });
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
        { tcp, _Socket, <<"ActionID: ", ActionID/binary>> } ->
            parse_event(Event#ami_event{ action_id = binary_to_list(erlami_helpers:trim(ActionID)) });
        { tcp, _Socket, <<"\r\n">> } ->
            Event;
        { tcp, _Socket, Msg } when is_binary(Msg) ->
            [Key, Value] = binary:split(erlami_helpers:trim(Msg), <<": ">>),
            parse_event(Event#ami_event{ data = [{Key, Value} | Event#ami_event.data] });
        Msg ->
            lager:error(<<"Unexpected message: ~p~n">>, [Msg]),
            fail
    end.
