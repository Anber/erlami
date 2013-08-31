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

-export( [
    start/0,
    request/2,
    originate/1,
    add_handler/2,
    add_handler/3,
    remove_handler/1
]).

-include("erlami.hrl").

start() -> start(erlami).

start(App) ->
    start_ok(App, application:start(App, permanent)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

-spec request(atom(), list({atom(), string()})) -> #ami_response{} | #ami_event{}.
request(Action, Data) ->
    gen_server:call(erlami_client, {request, Action, Data}). 

-spec originate(list({atom(), string()})) -> #ami_event{}.
originate(Data) ->
    #ami_response{uuid = ActionID} = request("Originate", [{'Async', "true"} | Data]),
    Handler = fun(#ami_event{name = 'OriginateResponse', action_id = EvActionID}) -> EvActionID =:= ActionID; (_) -> false end,
    erlami_events:wait(Handler).

-spec add_handler(atom(), fun((#ami_event{}) -> any())) -> reference()
                ;(fun((#ami_event{}) -> boolean()), fun((#ami_event{}) -> any())) -> reference().
add_handler(EventName, Callback) when is_atom(EventName), is_function(Callback, 1) ->
    Handler = fun(#ami_event{name = Name}) -> EventName =:= Name; (_) -> false end,
    add_handler(Handler, Callback);
add_handler(Handler, Callback) when is_function(Handler, 1), is_function(Callback, 1) ->
    add_handler(Handler, Callback, permanent).

add_handler(Handler, Callback, Type) when is_function(Handler, 1), is_function(Callback, 1) ->
    erlami_events:add_handler(Handler, Callback, Type).

-spec remove_handler(reference()) -> reference().
remove_handler(Ref) when is_reference(Ref) ->
    erlami_events:remove_handler(Ref).
