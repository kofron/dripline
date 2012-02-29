-module(dripline_sup).

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
    InstrSup = ?CHILD(dripline_instr_sup,supervisor),
    ConnMgr = ?CHILD(dripline_conn_mgr,worker),
    ConfMon = ?CHILD(dripline_conf_mon,worker),
    CmdMon  = ?CHILD(dripline_cmd_mon,worker),
    {ok, { SuperStrategy, [InstrSup,ConnMgr,ConfMon,CmdMon] }}.