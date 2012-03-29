%% @doc dripline_instr_sup is the OTP supervisor module that is 
%%		responsible for supervising all 'instrument' API modules.
%%		right now it is really pretty limited in the sense that
%%		the instruments are hard-coded in.
%% @version 0.1a
%% @todo We should be getting configuration data from the database
%%		itself and generating childspecs dynamically.
%% @author jared kofron <jared.kofron@gmail.com>
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
    timer:sleep(1000),
    InitC = lists:map(fun(X) ->
			      {ok,D} = dripline_conf_mgr:lookup_instr(X),
			      dripline_instr_data:to_childspec(D)
		      end,
		      dripline_conf_mgr:all_instr()),
    {ok, { SuperStrategy, InitC }}.
