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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts the port monitor.
-spec start(erlci_build()) -> {ok, pid()}.
start(Build) ->
  #{job := Job, build_number := BuildNumber} = Build,
  #{name := JobName} = Job,
  Name = list_to_atom(
    string:join([
      erlci_config:workspace_dir(),
      JobName,
      integer_to_list(BuildNumber)
    ], "_")
  ),
  gen_server:start({local, Name}, ?MODULE, [self(), Build], []).

%% @doc Creates (but not runs) a new build for the given job.
-spec create(erlci_job()) -> erlci_build().
create(Job) ->
  NextBuild = erlci_job:inc_build_number(Job),
  #{name := JobName} = Job,
  BuildHome = filename:join(
    [erlci_config:workspace_dir(), JobName, integer_to_list(NextBuild)]
  ),
  ok = erlci_file:create_dir(BuildHome),
  #{
    build_number => NextBuild,
    job => Job,
    home => BuildHome,
    phases => [],
    result => created
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc http://erlang.org/doc/man/gen_server.html#Module:init-1
-spec init([term()]) -> {ok, state()}.
init([Caller, Build]) ->
  Me = self(),
  #{job := Job} = Build,
  Caller ! {build_started, Me, Build},
  {ok, #{
    me => Me,
    job => Job,
    build => Build,
    caller => Caller
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
handle_cast(Message, State) ->
  lager:warning("Build got unknown msg: ~p", [Message]),
  {noreply, State}.

%% @doc http://erlang.org/doc/man/gen_server.html#Module:handle_info-2
-spec handle_info(
  term(), state()
) -> {noreply, state()} | {stop, term(), state()}.
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
  #{caller := Caller, build := Build, me := Me} = State,
  lager:debug("Build finished: ~p", [Reason]),
  Caller ! {build_finished, Me, Build},
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
