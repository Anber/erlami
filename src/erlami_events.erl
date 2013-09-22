%%% Erlami events module.
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

-export([add_handler/2, add_handler/3, add_handler/4, remove_handler/1, wait/1]).

-include_lib("erlami.hrl").

-record(state, {handlers = [], named_refs = []}).

init(_Args) ->
    {ok, #state{}}.

handle_call({add_handler, Handler, Callback, Ref, Type}, State = #state{handlers = Handlers}) ->
    {ok, Ref, State#state{handlers = [{Ref, Handler, Callback, Type} | Handlers]}};

handle_call({add_handler, Name, Handler, Callback, Ref, Type}, State = #state{handlers = Handlers, named_refs = Refs}) ->
    {ok, Ref, State#state{handlers = [{Ref, Handler, Callback, Type} | Handlers], named_refs = [{Name, Ref} | Refs]}};

handle_call({remove_handler, Ref}, State = #state{handlers = Handlers, named_refs = Refs}) when is_reference(Ref) ->
    NewHandlers = case lists:keyfind(Ref, 1, Handlers) of
        false -> Handlers;
        Handler -> lists:delete(Handler, Handlers) 
    end,
    NewRefs = case lists:keyfind(Ref, 2, Refs) of
        false -> Refs;
        R -> lists:delete(R, Refs)
    end,
    {ok, Ref, State#state{handlers = NewHandlers, named_refs = NewRefs}};

handle_call({remove_handler, Name}, State = #state{handlers = Handlers, named_refs = Refs}) when is_list(Name) ->
    {NewRefs, Ref} = case lists:keyfind(Name, 1, Refs) of
        false -> {Refs, null};
        {Name, R} -> {lists:delete({Name, R}, Refs), R}
    end,
    NewHandlers = case Ref of
        null -> Handlers;
        Ref ->
            case lists:keyfind(Ref, 1, Handlers) of
                false -> Handlers;
                Handler -> lists:delete(Handler, Handlers) 
            end
    end,
    {ok, Ref, State#state{handlers = NewHandlers, named_refs = NewRefs}};

handle_call(_Request, Waiters) ->
    Reply = ok,
    {ok, Reply, Waiters}.

handle_event(Event = #ami_event{}, State = #state{handlers = Handlers, named_refs = Refs}) ->
    F = fun({Ref, Handler, Callback, Type}) -> 
        case catch Handler(Event) of
            true ->
                if
                    is_pid(Callback) ->
                        Callback ! {Ref, Event};
                    is_function(Callback, 1) ->
                        Callback(Event)
                end, 
                case Type =:= permanent of
                    true -> true;
                    false ->
                        false
                end;
            _ -> true
        end
    end,
    NewHandlers = lists:filter(F, Handlers),
    Removed = Handlers -- NewHandlers,
    NewRefs = lists:filter(fun({Name, Ref}) ->
        lists:keyfind(Ref, 1, Removed) =:= false 
    end, Refs),
    {ok, State#state{handlers = NewHandlers, named_refs = NewRefs}};

handle_event(_Event, State) ->
    {ok, State}.

terminate(_, _State) -> ok.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

add_handler(Handler, Callback) ->
    add_handler(Handler, Callback, permanent).

add_handler(Handler, Callback, Type) ->
    Ref = make_ref(),
    gen_event:call(erlami_evm, ?MODULE, {add_handler, Handler, Callback, Ref, Type}).

add_handler(Name, Handler, Callback, Type) ->
    Ref = make_ref(),
    gen_event:call(erlami_evm, ?MODULE, {add_handler, Name, Handler, Callback, Ref, Type}).

wait(Handler) ->
    Ref = make_ref(),
    gen_event:call(erlami_evm, ?MODULE, {add_handler, Handler, self(), Ref, transient}),
    receive
        {Ref, Result} -> Result
    end.

remove_handler(Name) when is_list(Name)  ->
    gen_event:call(erlami_evm, ?MODULE, {remove_handler, Name});

remove_handler(Ref) when is_reference(Ref)  ->
    gen_event:call(erlami_evm, ?MODULE, {remove_handler, Ref}).
