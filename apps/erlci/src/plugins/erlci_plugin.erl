%%% @doc Plugin behavior.
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
-module(erlci_plugin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("include/erlci.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-callback run(
  erlci_job(),
  erlci_build(),
  erlci_phase_name(),
  erlci_step_config()
) -> erlci_step_result().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([proc_name/3, exec/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc For plugins that need to start themselves as a process, this will
%% return a unified name for it, taking into account the module, the job, and
%% the build information.
-spec proc_name(module(), erlci_job(), erlci_build()) -> atom().
proc_name(PluginModule, Job, Build) ->
  Module = atom_to_list(PluginModule),
  JobName = ?JOB:name(Job),
  BuildNumber = integer_to_list(?BUILD:number(Build)),
  list_to_atom(string:join([Module, JobName, BuildNumber], "_")).

%% @doc Called from plugins, intended to run an external command via shell
%% by using erlci_exec:start/1 and then monitor the process.
-spec exec(pid(), map()) -> {pid(), reference()}.
exec(BuildPid, ExecInfo) ->
  ?BUILD:log(BuildPid, info, "Running ~p", [ExecInfo]),
  {ok, Pid} = ?EXEC:start(ExecInfo),
  Ref = erlang:monitor(process, Pid),
  {Pid, Ref}.
