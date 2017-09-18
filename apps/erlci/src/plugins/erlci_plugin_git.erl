%%% @doc Git plugin.
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
-module(erlci_plugin_git).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-behavior(erlci_plugin_behavior).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("include/erlci.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type state():: map().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([
  start/0,
  init/1,
  handle_call/3,
  handle_info/2,
  handle_cast/2,
  code_change/3,
  terminate/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the plugin.
-spec start() -> {ok, pid()}.
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Runs the plugin.
-spec run(
  erlci_job(),
  erlci_phase_name(),
  erlci_step_config(),
  erlci_step_state()
) -> {erlci_build_status(), erlci_step_state()}.
run(_Job, _Phase, _StepConfig, _StepState) ->
  %gen_server:cast
  {success, #{}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc http://erlang.org/doc/man/gen_server.html#Module:init-1
-spec init([]) -> {ok, state()}.
init([]) ->
  {ok, #{
    monitor_refs => []
  }}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_call-3
-spec handle_call(
  term(), {pid(), term()}, state()
) -> {reply, term(), state()}.
handle_call(Message, _From, State) ->
  lager:warning("Got unknown request: ~p", [Message]),
  {reply, not_implemented, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Message, State) ->
  lager:warning("Got unknown msg: ~p", [Message]),
  {noreply, State}.

handle_info(Info, State) ->
  lager:warning("Got unknown msg: ~p", [Info]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:code_change-3
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:terminate-2
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.
