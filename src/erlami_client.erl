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

-record(state, {server_info, socket, pid}).

-spec start_link(list({atom(), any()})) -> {ok, pid()}.
start_link(ServerInfo) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ServerInfo, []).

-spec init(list({atom(), any()})) -> {ok, #state{}}.
init(ServerInfo) ->
    process_flag(trap_exit, true),
    connect(ServerInfo).

connect(ServerInfo) ->
    {host, Host} = lists:keyfind(host, 1, ServerInfo),
    {port, Port} = lists:keyfind(port, 1, ServerInfo),
    Socket = case gen_tcp:connect(Host, Port, [binary, {packet, line}]) of
        {ok, S} -> S;
        {error, Reason} ->
            lager:error("Can't connect to ~s:~b: ~s.", [Host, Port, inet:format_error(Reason)]),
            erlang:error(Reason, [{host, Host}, {port, Port}])  
    end,

    receive
        { tcp, Socket, <<"Asterisk Call Manager/", _Version:3/binary, "\r\n">> } ->
            {user, User} = lists:keyfind(user, 1, ServerInfo),
            {password, Password} = lists:keyfind(password, 1, ServerInfo),
            ok = auth(Socket, User, Password);
        { tcp, Socket, <<"Asterisk Call Manager/", _Version:5/binary, "\r\n">> } ->
            {user, User} = lists:keyfind(user, 1, ServerInfo),
            {password, Password} = lists:keyfind(password, 1, ServerInfo),
            ok = auth(Socket, User, Password);
        Msg ->
            lager:error(<<"Unexpected message: ~p~n">>, [Msg]),
            erlang:error(unexpected_message)
    end,

    Pid = spawn_link(?MODULE, loop, [Socket]), 
    gen_tcp:controlling_process(Socket, Pid),
    {ok, #state{server_info = ServerInfo, socket = Socket, pid = Pid}}.

-spec terminate(atom(), #state{}) -> ok.
terminate(_, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

handle_info({'EXIT', FromPID, tcp_closed}, #state{server_info = ServerInfo, pid = FromPID}) ->
    {ok, State} = connect(ServerInfo),
    lager:info(<<"Соединение с Asterisk Manager Interface восстановлено">>),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

-spec auth(gen_tcp:socket(), string(), string()) -> ok | fail.
auth(Socket, User, Password) ->
    LoginAction = #ami_action{
        name = login,
        fields = [{'Username', User}, {'Secret', Password}]
    },
    Response = request(Socket, LoginAction),
    case Response of
        #ami_response{success = true} ->
            ok;
        #ami_response{message = Msg} ->
            lager:error(<<"Ошибка авторизации в AMI: ~p~n">>, [Msg]),
            fail
    end.

-spec request(gen_tcp:socket(), #ami_action{}) -> #ami_response{}.
request(Socket, #ami_action{name = Name, id = ActionID, fields = Fields}) ->
    Cmd = lists:flatten([
        io_lib:format("Action: ~s\n", [Name]),
        io_lib:format("ActionID: ~s\n", [ActionID]),
        lists:map(fun ({K, V}) ->
            io_lib:format("~s: ~s\n", [K, V])
        end, Fields), 
        "\n"
    ]),
    gen_tcp:send(Socket, Cmd), 
    get_response().

loop(Socket) ->
    receive
        {action, Action = #ami_action{}, {Pid, Ref}} ->
            Response = request(Socket, Action),
            Pid ! {Ref, Response};
        {tcp, _Socket, <<"Event: ", Name/binary>>} ->
            EventName = binary_to_atom(erlami_helpers:trim(Name), utf8),
            Event = parse_event(#ami_event{ name = EventName }),
            erlami_evm:notify(Event); 
        {tcp_closed, _Socket} ->
            lager:warning(<<"Потеряно соединение с Asterisk Manager Interface">>),
            erlang:exit(tcp_closed);
        Msg ->
            lager:error(<<"Unexpected message: ~p">>, [Msg])
    after
        5000 ->
            request(Socket, #ami_action{name = ping})
    end,
    ?MODULE:loop(Socket).

handle_cast(_Request, State) ->
    {noreply, State}.

handle_call({action, Action = #ami_action{}}, _From, State = #state{pid = Pid}) ->
    Ref = make_ref(),
    Pid ! {action, Action, {self(), Ref}},
    Response = receive
        {Ref, Resp} -> Resp
    end,
    {reply, Response, State}.

parse_response(Response = #ami_response{fields = Fields}) when is_record(Response, ami_response)  ->
    receive
        { action, Action = #ami_action{}, {Pid, Ref}} ->
            self() ! { action, Action, {Pid, Ref}},
            parse_response(Response);
        { tcp, _Socket, <<"Message: ", Message/binary>> } ->
            parse_response(Response#ami_response{ message = erlami_helpers:trim(Message) });
        { tcp, _Socket, <<"ActionID: ", Uuid/binary>> } ->
            parse_response(Response#ami_response{ action_id = binary_to_list(erlami_helpers:trim(Uuid)) });
        { tcp, _Socket, <<"\r\n">> } ->
            Response;
        { tcp, _Socket, Msg } when is_binary(Msg) ->
            TrimmedMsg = erlami_helpers:trim(Msg),
            [Key, Value] = binary:split(TrimmedMsg, <<": ">>),
            Field = {binary_to_atom(Key, utf8), Value},
            % lager:warning(<<"Unknown field in response: ~p">>, [TrimmedMsg]),
            parse_response(Response#ami_response{ fields = [Field | Fields] });
        Msg ->
            lager:error(<<"Unexpected message: ~p">>, [Msg]),
            #ami_response{ success = false, message = io_lib:format(<<"Unexpected message ~p">>, [Msg]) }
    end.

get_response() ->
    receive
        { action, Action = #ami_action{}, {Pid, Ref}} ->
            self() ! { action, Action, {Pid, Ref}},
            get_response();
        {tcp, _Socket, <<"Event: ", Name/binary>>} ->
            EventName = binary_to_atom(erlami_helpers:trim(Name), utf8),
            Event = parse_event(#ami_event{ name = EventName }),
            erlami_evm:notify(Event),
            get_response();
        { tcp, _Socket, <<"Response: ", "Success", "\r\n">> } ->
            parse_response(#ami_response{ success = true });
        { tcp, _Socket, <<"Response: ", "Error", "\r\n">> } ->
            parse_response(#ami_response{ success = false });
        Msg ->
            lager:error(<<"Unexpected message: ~p">>, [Msg]),
            #ami_response{ success = false, message = io_lib:format(<<"Unexpected message ~p">>, [Msg]) }
    end.

parse_event_field(Event = #ami_event{}, {<<"ActionID">>, ActionID}) ->
    Event#ami_event{ action_id = binary_to_list(ActionID) };
parse_event_field(Event = #ami_event{}, {<<"Privilege">>, Privileges}) ->
    Event#ami_event{ privilege = [binary_to_atom(B, utf8) || B <- binary:split(Privileges, <<",">>)]};
parse_event_field(Event = #ami_event{}, {<<"Response">>, <<"Success">>}) ->
    Event#ami_event{ response = success };
parse_event_field(Event = #ami_event{}, {<<"Response">>, <<"Failure">>}) ->
    Event#ami_event{ response = failure };
parse_event_field(Event = #ami_event{}, {<<"Message">>, Message}) ->
    Event#ami_event{ message = Message };

parse_event_field(Event = #ami_event{channels = []}, {<<"Channel">>, Name}) ->
    Channel = #ami_ev_channel{name = Name},
    Event#ami_event{ channels = [Channel] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"Uniqueid">>, <<"<null>">>}) ->
    ModChan = Channel#ami_ev_channel{uniqueid = undefined},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"Uniqueid">>, Value}) ->
    ModChan = Channel#ami_ev_channel{uniqueid = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"UniqueID">>, <<"<null>">>}) ->
    ModChan = Channel#ami_ev_channel{uniqueid = undefined},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"UniqueID">>, Value}) ->
    ModChan = Channel#ami_ev_channel{uniqueid = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"ChannelState">>, Value}) ->
    ModChan = Channel#ami_ev_channel{channel_state = list_to_integer(binary_to_list(Value))},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"ChannelStateDesc">>, Value}) ->
    ModChan = Channel#ami_ev_channel{channel_state_desc = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"CallerIDNum">>, Value}) ->
    ModChan = Channel#ami_ev_channel{caller_id_num = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"CallerIDName">>, Value}) ->
    ModChan = Channel#ami_ev_channel{caller_id_name = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"ConnectedLineNum">>, Value}) ->
    ModChan = Channel#ami_ev_channel{connected_line_num = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"ConnectedLineName">>, Value}) ->
    ModChan = Channel#ami_ev_channel{connected_line_name = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"AccountCode">>, Value}) ->
    ModChan = Channel#ami_ev_channel{account_code = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"Context">>, Value}) ->
    ModChan = Channel#ami_ev_channel{context = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"Exten">>, Value}) ->
    ModChan = Channel#ami_ev_channel{exten = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };
parse_event_field(Event = #ami_event{channels = [Channel]}, {<<"Priority">>, Value}) ->
    ModChan = Channel#ami_ev_channel{priority = binary_to_list(Value)},
    Event#ami_event{ channels = [ModChan] };

parse_event_field(Event = #ami_event{fields = Fields}, {Key, Value}) ->
    Field = {binary_to_atom(Key, utf8), Value},
    % lager:warning(<<"Unknown field in event: ~p">>, [Field]),
    Event#ami_event{ fields = [Field | Fields] }.

parse_event(Event = #ami_event{}) ->
    receive
        { action, Action = #ami_action{}, {Pid, Ref}} ->
            self() ! { action, Action, {Pid, Ref}},
            parse_event(Event);
        { tcp, _Socket, <<"\r\n">> } ->
            Event;
        { tcp, _Socket, Msg } when is_binary(Msg) ->
            TrimmedMsg = erlami_helpers:trim(Msg),
            [Key, Value] = binary:split(TrimmedMsg, <<": ">>),
            parse_event(parse_event_field(Event, {Key, Value}));
        Msg ->
            lager:error(<<"Unexpected message: ~p~n">>, [Msg]),
            fail
    end.
