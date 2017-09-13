%%% @doc Handles yaml-related stuff.
%%%
%%% Copyright 2017 Marcelo Gornstein &lt;marcelog@@gmail.com&gt;
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
%%% @end
%%% @copyright Marcelo Gornstein <marcelog@gmail.com>
%%% @author Marcelo Gornstein <marcelog@gmail.com>
%%%
-module(erlci_yaml).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-include("include/erlci.hrl").

-export([read/1, field/2, field/3]).

-spec read(erlci_filename()) -> erlci_config().
read(Filename) ->
  [Doc|_] = yamerl_constr:file(Filename),
  Doc.

-spec field(erlci_config(), erlci_config_key()) -> erlci_config_value().
field(Doc, Name) ->
  field(Doc, Name, undefined).

-spec field(
  erlci_config(),
  erlci_config_key(),
  erlci_config_value()
) -> erlci_config_value().
field(Doc, Name, Default) ->
  proplists:get_value(Name, Doc, Default).
