%%% @doc Handles build-related stuff.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Includes.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-include("include/erlci.hrl").
-include_lib("kernel/include/file.hrl").
-import_record_info([{file, file_info}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([new/1]).
-export([create_build_directory/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new build.
-spec new(erlci_job()) -> erlci_build().
new(Job) ->
  #{home := JobHome} = Job,
  NextBuild = erlci_job:next_build_number(Job),
  Home = filename:join([JobHome, integer_to_list(NextBuild)]),
  #{
    job => Job,
    home => Home,
    phases => [],
    result => in_progress
  }.

-spec create_build_directory(erlci_build()) -> ok.
create_build_directory(Build) ->
  #{home := BuildHome} = Build,
  ok = create_dir(BuildHome).

-spec create_dir(erlci_directory()) -> ok.
create_dir(Path) ->
  [First|Components] = filename:split(Path),
  ok = create_dir(First, Components).

create_dir(Path, []) ->
  ok = test_and_create_dir(Path);

create_dir(Path, [Next|Components]) ->
  ok = test_and_create_dir(Path),
  create_dir(filename:join([Path, Next]), Components).

-spec test_and_create_dir(erlci_directory()) -> ok | not_a_dir.
test_and_create_dir(Path) ->
  case file:read_file_info(Path) of
    {ok, FileInfo} -> case FileInfo#file_info.type of
      directory -> ok;
      _ -> not_a_dir
    end;
    _ -> ok = file:make_dir(Path)
  end.
