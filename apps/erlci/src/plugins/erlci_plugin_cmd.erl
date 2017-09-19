%%% @doc Generic cmd plugin.
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
-module(erlci_plugin_cmd).
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
  run/4,
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
%% @doc Runs the plugin.
-spec run(
  erlci_job(),
  erlci_build(),
  erlci_phase_name(),
  erlci_step_config()
) -> erlci_step_result().
run(Job, Build, Phase, Config) ->
  gen_server:start(
    {local, ?MODULE}, ?MODULE, [self(), Job, Build, Phase, Config], []
  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc http://erlang.org/doc/man/gen_server.html#Module:init-1
-spec init([term()]) -> {ok, state()}.
init([BuildPid, Job, Build, Phase, Config]) ->
  self() ! run,
  {ok, #{
    build_pid => BuildPid,
    job => Job,
    build => Build,
    phase => Phase,
    config => Config,
    pid => undefined,
    ref => undefined
  }}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_call-3
-spec handle_call(
  term(), {pid(), term()}, state()
) -> {reply, term(), state()}.
handle_call(Message, _From, State) ->
  #{build_pid := BuildPid} = State,
  ?BUILD:log(BuildPid, warning, "Got unknown request: ~p", [Message]),
  {reply, not_implemented, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Message, State) ->
  #{build_pid := BuildPid} = State,
  ?BUILD:log(BuildPid, warning, "Got unknown msg: ~p", [Message]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_info-2
-spec handle_info(
  term(), state()
) -> {noreply, state()} | {stop, term(), state()}.
handle_info({exec_out, Line}, State) ->
  #{build_pid := BuildPid} = State,
  ?BUILD:log(BuildPid, info, Line, []),
  {noreply, State};

handle_info(
  {'DOWN', Ref, process, Pid, normal},
  State = #{ref := Ref, pid := Pid}
) ->
  {stop, normal, State};

handle_info(
  {'DOWN', Ref, process, Pid, {error, Code}},
  State = #{ref := Ref, pid := Pid}
) ->
  #{build_pid := BuildPid} = State,
  ?BUILD:log(BuildPid, error, "Shell failed with status code: ~p", [Code]),
  {stop, failed, State};

handle_info(run, State) ->
  #{build := Build, config := Config, build_pid := BuildPid} = State,
  Shell = ?YAML:field(Config, "shell"),
  Executable = ?YAML:field(Config, "executable"),
  Args = ?YAML:field(Config, "args"),

  NewExecutable = case Shell of
    undefined -> Executable;
    Shell_ -> Shell_
  end,

  ?BUILD:log(BuildPid, info, "Running: ~p with ~p", [NewExecutable, Args]),

  NewArgs = case Shell of
    undefined -> Args;
    _ -> ["-c", string:join([Executable|Args], " ")]
  end,

  ?BUILD:log(BuildPid, info, "Running: ~p with ~p", [NewExecutable, NewArgs]),

  {ok, Pid} = ?EXEC:start(#{
    cwd => ?BUILD:home(Build),
    command => NewExecutable,
    args => NewArgs,
    env => #{}
  }),
  Ref = erlang:monitor(process, Pid),
  {noreply, State#{
    pid := Pid,
    ref := Ref
  }};

handle_info(Info, State) ->
  #{build_pid := BuildPid} = State,
  ?BUILD:log(BuildPid, warning, "Got unknown msg: ~p", [Info]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:code_change-3
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:terminate-2
-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
  #{build := Build, job := Job, build_pid := BuildPid} = State,
  case Reason of
    normal -> BuildPid ! {step_done, success, Job, Build};
    E ->
      ?BUILD:log(BuildPid, error, "~p", [E]),
      BuildPid ! {step_done, failed, Job, Build}
  end,
  ok.
