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

-behaviour(supervisor).

-include_lib("kernel/include/inet.hrl").

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(
    CHILD(Name, Args),
    {Name,
        {erlami_client_worker, start_link, Args},
        permanent, 5000, worker, [?MODULE]
    }
).

%% ===================================================================
%% API functions
%% ===================================================================
start_link(Name, ServerInfo) ->
    supervisor:start_link({local, Name}, ?MODULE, ServerInfo).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init(ServerInfo) ->
    {host, Host} = lists:keyfind(host, 1, ServerInfo),
    {port, Port} = lists:keyfind(port, 1, ServerInfo),
    {user, User} = lists:keyfind(user, 1, ServerInfo),
    {password, Password} = lists:keyfind(password, 1, ServerInfo),
    {logfile, Logfile} = lists:keyfind(logfile, 1, ServerInfo),

    {ok, Socket} = gen_tcp:connect(Host, Port, [http]),


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
    Children = [],
    {ok, { {one_for_one, 5, 10}, Children} }.
