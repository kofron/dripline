B1;%% @doc dripline_data_log_sup is responsible for supervisiing all of the2c
%%		data loggers in the system.  it is directly responsible for the 
%%		permanent loggers and the supervisor for transient loggers, but
%%		does not directly control the transient loggers themselves.
%% @version 0.1a
%% @todo We should be getting configuration data from the database
%%		itself and generating childspecs dynamically.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dripline_data_log_sup).

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
    TestLogger = {
		tester,
		{
			dripline_data_logger,
			start_link,
			[<<"test1">>,5,infinity]
		},
		permanent,
		5000,
		worker,
		[dripline_data_logger]
	},
    {ok, { SuperStrategy, [TestLogger] }}.
