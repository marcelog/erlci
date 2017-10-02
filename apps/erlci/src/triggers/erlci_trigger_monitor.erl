%%% @doc Trigger monitor behavior.
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
-module(erlci_trigger_monitor).

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
  start_link/0,
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
%% @doc Starts and supervises the trigger.
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc http://erlang.org/doc/man/gen_server.html#Module:init-1
-spec init([term()]) -> {ok, state()}.
init([]) ->
  erlang:send_after(60000, self(), run),
  {ok, #{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(Message, _From, State) ->
  lager:warning("Trigger Monitor got unknown call: ~p", [Message]),
  {reply, not_implemented, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_cast-2
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(Message, State) ->
  lager:warning("Trigger Monitor got unknown cast: ~p", [Message]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_info-2
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(run, State) ->
  lager:debug("Trigger Monitor activating triggers", []),
  {ok, Dirs} = file:list_dir(?CFG:jobs_dir()),
  _ = [process(JobName) || JobName <- Dirs],
  erlang:send_after(60000, self(), run),
  {noreply, State};

handle_info(Info, State) ->
  lager:warning("Trigger Monitor got unknown info: ~p", [Info]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:code_change-3
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:terminate-2
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Process the given job by looking up its configured triggers and
%% running them.
-spec process(erlci_job_name()) -> ok.
process(JobName) ->
  Job = ?JOB:load(JobName),
  Triggers = maps:to_list(?JOB:triggers(Job)),
  _ = run_triggers(Job, Triggers),
  ok.

%% @doc Runs all triggers for the given job.
-spec run_triggers(erlci_job(), proplists:proplist()) -> ok.
run_triggers(Job, Triggers) ->
  _ = [
    run_trigger(Job, TriggerType, Config)
    || {TriggerType, Config}
    <- Triggers
  ],
  ok.

%% @doc Runs the given trigger type with the given config for the job.
-spec run_trigger(erlci_job(), string(), proplists:proplist()) -> ok.
run_trigger(Job, TriggerType, Config) ->
  JobName = ?JOB:name(Job),
  _ = case erlci_build_monitor:build_is_running(JobName) of
    false ->
      TriggerModule = list_to_atom(
        string:join(["erlci", "trigger", TriggerType], "_")
      ),
      %% @todo what should we do with this pid.. ?
      try
        case erlang:apply(TriggerModule, start, [Job, Config]) of
          {start_build, Reason, Description} ->
            BuildDescription = ?BUILD:describe_build(
              "trigger", atom_to_list(TriggerModule), Reason, Description
            ),
            erlci_build_monitor:start_build(JobName, BuildDescription);
          skip -> lager:debug("Skipping ~p for ~p", [TriggerType, JobName])
        end
      catch
        _:E -> lager:debug(
          "Error in trigger ~p for ~p: ~p", [TriggerType, JobName, E]
        )
      end;
    {true, Build} -> lager:debug(
      "Skipping ~p for ~p since build (~p) is already running", [
        TriggerType, ?JOB:name(Job), ?BUILD:number(Build)
      ]
    )
  end,
  ok.

