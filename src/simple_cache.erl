%%% @doc Main module for simple_cache.
%%%
%%% Copyright 2013 Marcelo Gornstein &lt;marcelog@@gmail.com&gt;
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
-module(simple_cache).
-author('marcelog@gmail.com').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(ETS_TID, atom_to_list(?MODULE)).
-define(NAME(N), list_to_atom(?ETS_TID ++ "_" ++ atom_to_list(N))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API.
-export([init/1]).
-export([get/4]).
-export([flush/1, flush/2, flush/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Initializes a cache.
-spec init(atom()) -> ok.
init(CacheName) ->
  RealName = ?NAME(CacheName),
  RealName = ets:new(RealName, [
    named_table, {read_concurrency, true}, public, {write_concurrency, true}
  ]),
  ok.

%% @doc Deletes the keys that match the given ets:matchspec() from the cache.
-spec flush(atom(), term()) -> true.
flush(CacheName, Key) ->
  RealName = ?NAME(CacheName),
  ets:delete(RealName, Key).

%% @doc Deletes all keys in the given cache.
-spec flush(atom()) -> true.
flush(CacheName) ->
  RealName = ?NAME(CacheName),
  true = ets:delete_all_objects(RealName).

%% @doc Deletes a value from cache only if it still has the expected Expiry
%% value.  This is used by the expirer as way to avoid deleting a value that
%% has since been updated (it would thus have a different Expiry).
-spec flush(atom(), term(), pos_integer()) -> true.
flush(CacheName, Key, Expiry) ->
  RealName = ?NAME(CacheName),
  ets:match_delete(RealName, {Key, '_', Expiry}).

%% @doc Tries to lookup Key in the cache, and execute the given FunResult
%% on a miss.
-spec get(atom(), infinity|pos_integer(), term(), function()) -> term().
get(CacheName, LifeTime, Key, FunResult) ->
  RealName = ?NAME(CacheName),
  case ets:lookup(RealName, Key) of
    [] ->
      % Not found, create it.
      V = FunResult(),
      set(CacheName, LifeTime, Key, V),
      V;
    [{Key, R}] -> R % Found, return the value.
  end.

%% @doc Sets Key in the cache to the given Value
set(CacheName, LifeTime, Key, Value) ->
  RealName = ?NAME(CacheName),
  %% Store the expiry time on the entry itself so that the expirer won't
  %% accidentally expire the entry early if it gets updated between now
  %% and when the expiration is first scheduled to occur.
  Expiry = now_millis() + LifeTime,
  ets:insert(RealName, {Key, Value, Expiry}),
  erlang:send_after(
    LifeTime, simple_cache_expirer, {expire, Key, Expiry}
  ),
  ok.

now_millis() ->
    {Mega, Seconds, Micro} = os:timestamp(),
    (Mega * 1000000 + Seconds) * 1000 + trunc(Micro/1000).
