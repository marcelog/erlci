%%% @doc Handles job-related stuff.
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
-module(erlci_job).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("include/erlci.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([load/1]).
-export([inc_build_number/1]).
-export([name/1, home/1, steps/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Loads a job from its config file located inside its base home directory.
-spec load(erlci_job_name()) -> erlci_job().
load(JobName) ->
  BaseDir = ?CFG:jobs_dir(),
  from_file(filename:join([BaseDir, JobName, "config.yml"])).

%% @doc Increments the build number of a given job.
-spec inc_build_number(erlci_job()) -> pos_integer().
inc_build_number(Job) ->
  File = build_number_file(Job),
  Next = next_build_number(Job),
  ok = file:write_file(File, integer_to_binary(Next)),
  Next.

%% @doc Returns all the steps that are to be executed in order for the given
%% phase.
-spec steps(erlci_job(), erlci_phase_name()) -> [erlci_step()].
steps(Job, PhaseString) ->
  CurrentPhases = phases(Job),
  %PhaseString = erlang:atom_to_list(Phase),
  case maps:get(PhaseString, CurrentPhases, phase_not_found) of
    phase_not_found -> [];
    Steps_ -> Steps_
  end.

%% @doc Returns the name of the job.
-spec name(erlci_job()) -> erlci_job_name().
name(Job) ->
  #{name := Name} = Job,
  Name.

%% @doc Returns the home of the job.
-spec home(erlci_job()) -> erlci_job_home().
home(Job) ->
  #{home := Home} = Job,
  Home.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Loads a job from a YAML file.
-spec from_file(erlci_filename()) -> erlci_job().
from_file(Filename) ->
  Doc = erlci_yaml:read(Filename),
  JobName = erlci_yaml:field(Doc, "name"),
  Job = new(
    JobName,
    erlci_yaml:field(Doc, "description"),
    filename:join(?CFG:jobs_dir(), JobName)
  ),
  NewJob = lists:foldl(
    fun({Phase, Steps}, Acc) ->
      lists:foldl(
        fun({StepName, StepInfo}, Acc2) ->
          StepConfig = erlci_yaml:field(StepInfo, "config"),
          StepType = erlci_yaml:field(StepInfo, "type"),
          add_step(Acc2, Phase, StepType, StepName, StepConfig)
        end,
        Acc,
        Steps
      )
    end,
    Job,
    erlci_yaml:field(Doc, "phases")
  ),
  NewJob.

%% @doc Creates a new job structure.
-spec new(
  erlci_job_name(), erlci_job_description(), erlci_job_home()
) -> erlci_job().
new(Name, Description, Home) ->
  #{
    name => Name,
    description => Description,
    home => Home,
    phases => #{},
    variables => #{}
  }.

%% @doc Returns all the phases available for the given job.
-spec phases(erlci_job()) -> map().
phases(Job) ->
  #{phases := Phases} = Job,
  Phases.

%% @doc Adds (appends) a new step to the given phase of the given job.
-spec add_step(
  erlci_job(),
  erlci_phase_name(),
  erlci_step_type(),
  erlci_step_name(),
  erlci_step_config()
) -> erlci_job().
add_step(Job, Phase, StepType, StepInstanceName, Config) ->
  Step = erlci_step:new(StepInstanceName, StepType, Config),

  CurrentPhases = phases(Job),
  CurrentSteps = steps(Job, Phase),

  NewSteps = lists:reverse([Step|CurrentSteps]),
  NewPhase = maps:put(Phase, NewSteps, CurrentPhases),

  Job#{phases := NewPhase}.

%% @doc Returns the next build number for this job.
-spec next_build_number(erlci_job()) -> erlci_build_number().
next_build_number(Job) ->
  Name = name(Job),
  File = build_number_file(Job),
  lager:debug("Incrementing build number for: ~p at ~p", [Name, File]),
  case file:read_file(File) of
    {ok, N} -> binary_to_integer(N) + 1;
    _ -> 1
  end.

%% @doc Returns the full absolute path to the build number file of a given job.
-spec build_number_file(erlci_job()) -> erlci_filename().
build_number_file(Job) ->
  JobHome = home(Job),
  filename:join([JobHome, "last_build.txt"]).
