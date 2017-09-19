%%% @doc gen_server to exec/monitor external programs.
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
-module(erlci_exec).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type state():: map().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([
  start/1,
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
%% @doc Starts the port monitor.
-spec start(map()) -> {ok, pid()}.
start(ExecInfo) ->
  gen_server:start(?MODULE, [ExecInfo, self()], []).

%% @doc http://erlang.org/doc/man/gen_server.html#Module:init-1
-spec init([term()]) -> {ok, state()}.
init([ExecInfo, Caller]) ->
  lager:debug("Port monitor started"),
  #{
    cwd := Cwd,
    command := Command,
    args := Args,
    env := Env
  } = ExecInfo,
  EnvList = maps:to_list(Env),
  Opts = [
    {env, EnvList},
    {cd, Cwd},
    {line, 4096},
    {args, Args},
    exit_status,
    use_stdio,
    stderr_to_stdout,
    eof,
    {parallelism, true}
  ],
  Port = open_port({spawn_executable, Command}, Opts),
  {ok, #{port => Port, caller => Caller}}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_call-3
-spec handle_call(
  term(), {pid(), term()}, state()
) -> {reply, term(), state()}.
handle_call(Message, _From, State) ->
  lager:warning("Port Monitor got unknown request: ~p", [Message]),
  {reply, not_implemented, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Message, State) ->
  lager:warning("Port Monitor got unknown msg: ~p", [Message]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_info-2
-spec handle_info(
  term(), state()
) -> {noreply, state()} | {stop, term(), state()}.
handle_info(
  {Port, {data, {_EolNoEol, Data}}}, State = #{port := Port, caller := Caller}
) ->
  Caller ! {exec_out, Data},
  {noreply, State};

handle_info({Port, {exit_status, 0}}, State = #{port := Port}) ->
  {stop, normal, State};

handle_info({Port, {exit_status, Code}}, State = #{port := Port}) ->
  {stop, {error, Code}, State};

handle_info({Port, eof}, State = #{port := Port}) ->
  {noreply, State};

handle_info(Info, State) ->
  lager:warning("Port Monitor got unknown msg: ~p", [Info]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:code_change-3
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:terminate-2
-spec terminate(term(), state()) -> ok.
terminate(Reason, _State) ->
  lager:debug("Port finished: ~p", [Reason]),
  ok.
