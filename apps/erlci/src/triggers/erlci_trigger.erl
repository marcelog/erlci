%%% @doc Trigger behavior.
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
-module(erlci_trigger).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("include/erlci.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callbacks.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-callback start(
  erlci_job(),
  erlci_trigger_config()
) -> erlci_trigger_result().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([proc_name/2, exec/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc For triggers that need to start themselves as a process, this will
%% return a unified name for it, taking into account the module and the job.
-spec proc_name(module(), erlci_job()) -> atom().
proc_name(TriggerModule, Job) ->
  Module = atom_to_list(TriggerModule),
  JobName = ?JOB:name(Job),
  list_to_atom(string:join([Module, JobName], "_")).

%% @doc Called from triggers, intended to run an external command via shell
%% by using erlci_exec:start/1 and then monitor the process.
-spec exec(map()) -> {ok, [string()]} | {error, term(), [string()]}.
exec(ExecInfo) ->
  lager:info("Running ~p", [ExecInfo]),
  {ok, Pid} = ?EXEC:start(ExecInfo),
  Ref = erlang:monitor(process, Pid),
  exec_wait(Ref, Pid, []).

%% @doc Waits for the end of the command.
-spec exec_wait(
  reference(), pid(), [string()]
) -> {ok, [string()]} | {error, term(), [string()]}.
exec_wait(Ref, Pid, Acc) ->
  receive
    {exec_out, Line} ->
      exec_wait(Ref, Pid, [Line|Acc]);
    {'DOWN', Ref, process, Pid, normal} ->
      {ok, lists:reverse(Acc)};
    {'DOWN', Ref, process, Pid, {error, Code}} ->
      {error, Code, lists:reverse(Acc)}
  end.
