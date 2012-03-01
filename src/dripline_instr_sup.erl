-module(dripline_instr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SuperStrategy = {one_for_one, 5, 10},
    SwitchMux = {
		switch_mux,
		{
			agilent34970a,
			start_link,
			[switch_mux,left_box,8]
		},
		permanent,
		5000,
		worker,
		[agilent34790a]
	},
	ADCMux = {
		adc_mux,
		{
			agilent34970a,
			start_link,
			[adc_mux,left_box,9]
		},
		permanent,
		5000,
		worker,
		[agilent34790a]
	},
    {ok, { SuperStrategy, [SwitchMux,ADCMux] }}.