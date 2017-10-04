%%% @doc Git polling trigger.
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
-module(erlci_trigger_git_poll).
-author("marcelog@gmail.com").
-github("https://github.com/marcelog").
-homepage("http://marcelog.github.com/").
-license("Apache License 2.0").

-behavior(erlci_trigger).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("include/erlci.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Runs this trigger.
-spec start(
  erlci_job(),
  erlci_trigger_config()
) -> erlci_trigger_result().
start(Job, Config) ->
  Expression = ?YAML:field(Config, "expression"),
  case erl_vcron:applies(calendar:local_time(), Expression) of
    true ->
      RevisionFile = last_revision_file(Job),
      case file:read_file(RevisionFile) of
        {ok, Revision} -> process(Job, Config, binary_to_list(Revision));
        {error, enoent} -> process(Job, Config, "not_existant");
        {error, Error} ->
          lager:error("Could not read: ~p (~p)", [RevisionFile, Error]),
          skip
      end;
    false -> skip
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Checks if a build needs to be triggered by comparing last revisions on
%% remote/master and local/last build.
-spec process(
  erlci_job(), erlci_trigger_config(), string()
) -> erlci_trigger_result().
process(Job, Config, Revision) ->
  JobName = ?JOB:name(Job),
  Branch = "refs/heads/master",
  Executable = ?YAML:field(Config, "executable"),
  Repo = ?YAML:field(Config, "repository"),
  SourceDir = ?YAML:field(Config, "source_directory"),
  ExecInfo = #{
    cwd => ?JOB:filename(Job, SourceDir),
    command => Executable,
    args => ["ls-remote", "-h", "-t", Repo],
    env => #{}
  },
  case ?TRIGGER:exec(ExecInfo) of
    {ok, Output} ->
      case find_revision(Output, Branch) of
        undefined ->
          lager:error("Could not find last revision for ~p", [Branch]),
          skip;
        Revision ->
          lager:info(
            "Same revision ~p for ~p, not building", [Revision, JobName
          ]),
          skip;
        NewRevision ->
          Description = lists:flatten(io_lib:format(
            "From ~p to ~p", [Revision, NewRevision]
          )),
          ok = file:write_file(last_revision_file(Job), NewRevision),
          {start_build, "Revision Changed", Description}
      end;
    {error, Code, Output} ->
      lager:error("Git Polling with ~p: ~p (~p)", [ExecInfo, Output, Code]),
      skip
  end.

%% @doc Finds the revision of the given branch after a git ls-remote.
-spec find_revision([string()], string()) -> undefined | string().
find_revision([], _Branch) ->
  undefined;

find_revision([Line|Output], Branch) ->
  case string:tokens(Line, "\t") of
    [Revision, Branch] -> Revision;
    [_Revision, _ABranch] -> find_revision(Output, Branch)
  end.

%% @doc Returns the path to the last revision file.
-spec last_revision_file(erlci_job()) -> erlci_filename().
last_revision_file(Job) ->
  ?JOB:filename(Job, "last_git_revision.txt").
