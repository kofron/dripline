
-module(dripline_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, 
			 permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    DriplineHWSup = {dripline_ios_card_sup,
		     {dripline_ios_card_sup,start_link,[]},
		     permanent,
		     brutal_kill,
		     supervisor,
		     [dripline_ios_card_sup]},
    DriplineMMSup = {dripline_meterman_sup,
		     {dripline_meterman_sup,start_link,[]},
		     permanent,
		     brutal_kill,
		     supervisor,
		     [dripline_meterman_sup]},
    {ok, { {one_for_one, 5, 10}, [DriplineHWSup,DriplineMMSup]} }.

