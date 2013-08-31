%%% Originate AMI action.
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
-module(erlami_action_originate).

-author("Anton Evzhakov <anber@anber.ru>").
-license("Apache License 2.0").

-include("erlami.hrl").

-export([do/3]).

parse_options([{timeout, V} | Tail], Fields) when is_integer(V) ->
    parse_options(Tail, [{'Timeout', integer_to_list(V)} | Fields]);
parse_options([{caller_id, V} | Tail], Fields) ->
    parse_options(Tail, [{'CallerID', V} | Fields]);
parse_options([], Fields) -> Fields.

parse_to({app, App, Data}) ->
    [{'Application', App}, {'Data', Data}].

do(From, To, Options) ->
    OptFields = parse_options(Options, []),
    ToFields = parse_to(To),
    Action = #ami_action{
        name = 'Originate',
        fields = [{'Async', "true"}, {'Channel', From}] ++ ToFields ++ OptFields
    },
    #ami_response{action_id = ActionID} = erlami:action(Action),
    Handler = fun(#ami_event{name = 'OriginateResponse', action_id = EvActionID}) -> EvActionID =:= ActionID; (_) -> false end,
    erlami_events:wait(Handler).
