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
-module(erlami_events).

-author("Anton Evzhakov <anber@anber.ru>").
-license("Apache License 2.0").
-behaviour(gen_event).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-export([add_handler/2, add_handler/3, remove_handler/1, wait/1]).

-include_lib("erlami.hrl").

init(_Args) ->
    {ok, []}.

handle_call({add_handler, Handler, Callback, Ref, Type}, Handlers) ->
    {ok, Ref, [{Ref, Handler, Callback, Type} | Handlers]};

handle_call({remove_handler, Ref}, Handlers) ->
    NewHandlers = case lists:keyfind(Ref, 1, Handlers) of
        false -> Handlers;
        Handler -> lists:delete(Handler, Handlers) 
    end,
    {ok, Ref, NewHandlers};

handle_call(_Request, Waiters) ->
    Reply = ok,
    {ok, Reply, Waiters}.

handle_event(Event = #ami_event{}, Handlers) ->
    F = fun({Ref, Handler, Callback, Type}) -> 
        case catch Handler(Event) of
            true ->
                if
                    is_pid(Callback) ->
                        Callback ! {Ref, Event};
                    is_function(Callback, 1) ->
                        Callback(Event)
                end, 
                Type =:= permanent;
            _ -> true
        end
    end,
    {ok, lists:filter(F, Handlers)};

handle_event(_Event, Handlers) ->
    {ok, Handlers}.

terminate(_, _State) -> ok.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

add_handler(Handler, Callback) ->
    add_handler(Handler, Callback, permanent).

add_handler(Handler, Callback, Type) ->
    Ref = make_ref(),
    gen_event:call(erlami_evm, ?MODULE, {add_handler, Handler, Callback, Ref, Type}).

wait(Handler) ->
    Ref = make_ref(),
    gen_event:call(erlami_evm, ?MODULE, {add_handler, Handler, self(), Ref, transient}),
    receive
        {Ref, Result} -> Result
    end.

remove_handler(Ref) when is_reference(Ref)  ->
    gen_event:call(erlami_evm, ?MODULE, {remove_handler, Ref}).
