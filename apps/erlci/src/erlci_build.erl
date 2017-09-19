%%% @doc Handles build-related stuff.
%%%
%%% @todo Try a gen_statem for this.
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
-module(erlci_build).
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
%%% Includes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("include/erlci.hrl").

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

-export([create/1]).
-export([log/4]).
-export([home/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns the workspace/home directory for the given build.
-spec home(erlci_build()) -> erlci_directory().
home(Build) ->
  #{home := Home} = Build,
  Home.

%% @doc Starts the port monitor.
-spec start(erlci_build()) -> {ok, pid()}.
start(Build) ->
  #{job := Job, build_number := BuildNumber} = Build,
  JobName = ?JOB:name(Job),
  Name = list_to_atom(
    string:join([
      JobName,
      integer_to_list(BuildNumber)
    ], "_")
  ),
  gen_server:start({local, Name}, ?MODULE, [self(), Build], []).

-spec log(pid(), atom(), string(), [term()]) -> ok.
log(BuildPid, Level, Msg, Args) ->
  gen_server:cast(BuildPid, {log, Level, Msg, Args}).

%% @doc Creates (but not runs) a new build for the given job.
-spec create(erlci_job()) -> erlci_build().
create(Job) ->
  NextBuild = ?JOB:inc_build_number(Job),
  JobName = ?JOB:name(Job),
  BuildHome = filename:join(
    [?CFG:workspace_dir(), JobName, integer_to_list(NextBuild)]
  ),
  Logfile = filename:join([BuildHome, "log.txt"]),
  ok = erlci_file:create_dir(BuildHome),
  #{
    build_number => NextBuild,
    log => Logfile,
    job => Job,
    home => BuildHome,
    phases => [],
    status => created
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc http://erlang.org/doc/man/gen_server.html#Module:init-1
-spec init([term()]) -> {ok, state()}.
init([Caller, Build]) ->
  Me = self(),
  #{job := Job, log := Log} = Build,
  Hash = erlang:phash2(Build),
  ok = lager:md([{build_hash, Hash}]),
  {ok, Trace} = lager:trace_file(Log, [{build_hash, Hash}], debug),
  Caller ! {build_started, Me, Build},
  Me ! {start},
  {ok, #{
    me => Me,
    build_hash => Hash,
    build_trace => Trace,
    job => Job,
    build => Build,
    caller => Caller,
    current_phase => undefined,
    current_step_module => undefined,
    current_step_pid => undefined,
    current_step_ref => undefined,
    phases => [],
    steps => []
  }}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_call-3
-spec handle_call(
  term(), {pid(), term()}, state()
) -> {reply, term(), state()}.
handle_call(Message, _From, State) ->
  lager:warning("Build got unknown request: ~p", [Message]),
  {reply, not_implemented, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({log, debug, Msg, Args}, State) ->
  lager:debug(Msg, Args),
  {noreply, State};

handle_cast({log, info, Msg, Args}, State) ->
  lager:info(Msg, Args),
  {noreply, State};

handle_cast({log, warning, Msg, Args}, State) ->
  lager:warning(Msg, Args),
  {noreply, State};

handle_cast({log, error, Msg, Args}, State) ->
  lager:error(Msg, Args),
  {noreply, State};

handle_cast(Message, State) ->
  lager:warning("Build got unknown msg: ~p", [Message]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_info-2
-spec handle_info(
  term(), state()
) -> {noreply, state()} | {stop, term(), state()}.
handle_info({start}, State) ->
  #{job := Job, build := Build} = State,
  JobName = ?JOB:name(Job),
  self() ! {next_phase},
  lager:debug("Running phases for ~p", [JobName]),
  NewBuild = status_in_progress(Build),
  {noreply, State#{phases := ?PHASES, build := NewBuild}};

handle_info({next_phase}, State = #{phases := []}) ->
  #{job := Job, build := Build} = State,
  JobName = ?JOB:name(Job),
  lager:debug("No more phases for ~p", [JobName]),
  {stop, normal, State#{build := status_success(Build)}};

handle_info({next_phase}, State) ->
  #{job := Job, phases := [Phase|NextPhases]} = State,
  JobName = ?JOB:name(Job),
  lager:debug("Running phase ~p for ~p", [Phase, JobName]),
  Steps = ?JOB:steps(Job, Phase),
  self() ! {next_step},
  {noreply, State#{
    steps := Steps,
    current_phase := Phase,
    phases := NextPhases
  }};

handle_info({next_step}, State = #{steps := []}) ->
  #{job := Job, current_phase := CurrentPhase} = State,
  JobName = ?JOB:name(Job),
  lager:debug("No more steps for ~p of ~p", [CurrentPhase, JobName]),
  self() ! {next_phase},
  {noreply, State#{steps := [], current_phase := undefined}};

handle_info({next_step}, State) ->
  #{
    job := Job,
    current_phase := CurrentPhase,
    steps := [Step|NextSteps],
    build := Build
  } = State,
  JobName = ?JOB:name(Job),
  #{
    name := StepName,
    type := StepType,
    config := StepConfig
  } = Step,
  lager:debug("Starting ~p:~p for ~p", [CurrentPhase, StepName, JobName]),
  StepModule = list_to_atom(string:join(["erlci", "plugin", StepType], "_")),
  case erlang:apply(StepModule, run, [Job, Build, CurrentPhase, StepConfig]) of
    {BuildStatus, NewJob, NewBuild} ->
      handle_info(
        {step_done, BuildStatus, NewJob, NewBuild},
        State#{
          current_step_pid := none,
          current_step_ref := none,
          steps := NextSteps
        }
      );
    {ok, StepPid} ->
      StepRef = erlang:monitor(process, StepPid),
      {noreply, State#{
        steps := NextSteps,
        current_step_module := StepModule,
        current_step_pid := StepPid,
        current_step_ref := StepRef
      }};
    StepError ->
      lager:error("Step returned ~p", [StepError]),
      {stop, normal, State#{
        build := status_failed(Build)
      }}
  end;

handle_info(
  {step_done, BuildStatus, NewJob, NewBuild}, State
) ->
  case BuildStatus of
    success ->
      self() ! {next_step},
      {noreply, State#{
        build := NewBuild,
        job := NewJob
      }};
    failed ->
      {stop, failed, State#{
        build := status_failed(NewBuild),
        job := NewJob
      }}
  end;

handle_info(
  {'DOWN', StepRef, process, StepPid, normal},
  State = #{current_step_ref := StepRef, current_step_pid := StepPid}
) ->
  {noreply, State};

handle_info(
  {'DOWN', StepRef, process, StepPid, Status},
  State = #{current_step_ref := StepRef, current_step_pid := StepPid}
) ->
  #{build := Build} = State,
  lager:debug("Step finished with error ~p", [Status]),
  {stop, failed, State#{
    build := status_failed(Build)
  }};

handle_info(Info, State) ->
  lager:warning("Build got unknown msg: ~p", [Info]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:code_change-3
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:terminate-2
-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
  #{
    caller := Caller,
    build := Build,
    me := Me,
    build_trace := Trace,
    build_hash := Hash
  } = State,
  lager:debug([{build_hash, Hash}], "Build finished: ~p", [Reason]),
  ok = lager:stop_trace(Trace),
  Caller ! {build_finished, Me, Build},
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Returns the log filename for the given build.
%-spec log(erlci_build()) -> erlci_filename().
%log(Build) ->
%  Home = home(Build),
%  filename:join([Home, "log.txt"]).
%
%% @doc Returns the home directory of the build.
%-spec home(erlci_build()) -> erlci_directory().
%home(Build) ->
%  #{home := Home} = Build,
%  Home.

%% @doc Marks the build as in progress.
-spec status_in_progress(erlci_build()) -> erlci_build().
status_in_progress(Build) ->
  update_status(Build, in_progress).

%% @doc Marks the build as failed.
-spec status_failed(erlci_build()) -> erlci_build().
status_failed(Build) ->
  update_status(Build, failed).

%% @doc Marks the build as success.
-spec status_success(erlci_build()) -> erlci_build().
status_success(Build) ->
  update_status(Build, success).

%% @doc Changes the build status
-spec update_status(erlci_build(), erlci_build_status()) -> erlci_build().
update_status(Build, Status) ->
  Build#{status := Status}.
