-module(dl_cdb_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    {ok, Pools} = application:get_env(dl_core, worker_pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
                  PoolArgs = [{name, {local, Name}},
                          {worker_module, dl_cdb_worker}]
                      ++ SizeArgs,
                  poolboy:child_spec(Name, PoolArgs, WorkerArgs)
              end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
