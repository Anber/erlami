%%% A AMI event manager.
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
-module(erlami_evm).
-author("Anton Evzhakov <anber@anber.ru>").
-license("Apache License 2.0").

%% API
-export([start_link/0, add_handler/1, notify/1]).

start_link() ->
    {ok, Pid} = gen_event:start_link({local, ?MODULE}),
    gen_event:add_handler(?MODULE, erlami_events, []),
    {ok, Pid}.

add_handler(Module) ->
    gen_event:add_handler(?MODULE, Module, []).

notify(Event) ->
    gen_event:notify(?MODULE, Event).
