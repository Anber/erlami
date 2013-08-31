%%% Main supervisor for the erlami application.
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
-module(erlami_sup).
-author("Anton Evzhakov <anber@anber.ru>").
-license("Apache License 2.0").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(ServerInfo) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, ServerInfo).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(ServerInfo) ->
    Children = [
        {erlami_evm, {erlami_evm, start_link, []}, permanent, infinity, worker, [?MODULE]},
        {erlami_client, {erlami_client, start_link, [ServerInfo]}, permanent, infinity, worker, [?MODULE]}
    ],
    {ok, { {one_for_one, 5, 10}, Children} }.

