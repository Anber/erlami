%%% Main erlami_helpers module.
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
-module(erlami_helpers).

-author("Anton Evzhakov <anber@anber.ru>").
-license("Apache License 2.0").

-include("erlami.hrl").

-export( [
    trim/1
]).

trim(Data) when is_binary(Data) ->
    DataSize = byte_size(Data) - 2,
    <<Trimed:DataSize/binary, "\r\n">> = Data,
    Trimed.
