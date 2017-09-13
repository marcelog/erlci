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

-export([new/3, from_file/1, add_step/5]).

from_file(Filename) ->
  Doc = erlci_yaml:read(Filename),
  Job = new(
    erlci_yaml:field(Doc, "name"),
    erlci_yaml:field(Doc, "description"),
    ""
  ),
  NewJob = lists:foldl(
    fun({Phase, Steps}, Acc) ->
      lists:foldl(
        fun({StepName, StepInfo}, Acc2) ->
          StepConfig = erlci_yaml:field(StepInfo, "config"),
          StepType = erlci_yaml:field(StepInfo, "type"),
          erlci_job:add_step(Acc2, Phase, StepType, StepName, StepConfig)
        end,
        Acc,
        Steps
      )
    end,
    Job,
    erlci_yaml:field(Doc, "phases")
  ),
  NewJob.

new(Name, Description, Home) ->
  #{
    name => Name,
    description => Description,
    home => Home,
    phases => #{},
    variables => #{}
  }.

add_step(Job, Phase, StepType, StepInstanceName, Config) ->
  Step = erlci_step:new(StepInstanceName, StepType, Config),

  CurrentPhases = get_phases(Job),
  CurrentSteps = get_steps(Job, Phase),

  NewSteps = lists:reverse([Step|CurrentSteps]),
  NewPhase = maps:put(Phase, NewSteps, CurrentPhases),

  Job#{phases := NewPhase}.

get_phases(Job) ->
  #{phases := Phases} = Job,
  Phases.

get_steps(Job, Phase) ->
  CurrentPhases = get_phases(Job),
  case maps:get(Phase, CurrentPhases, phase_not_found) of
    phase_not_found -> [];
    Steps_ -> Steps_
  end.

