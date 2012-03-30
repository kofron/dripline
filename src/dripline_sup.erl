%% @doc dripline_sup is the top-level supervisor for the dripline
%%		application.  It starts all of the managers and monitors
%%		that listen to couchdb for changes and manage state.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
%% @todo Once we start getting config data from the database itself,
%% 		we're going to need to be more sophisticated than this in terms
%% 		of the tree structure.

-module(dripline_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {
							I, 
							{I, start_link, []}, 
							permanent, 
							5000, 
							Type, 
							[I]
						}).

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
    Compiler = ?CHILD(dripline_compiler,worker),
    ConnMgr = ?CHILD(dripline_conn_mgr,worker),
    ConfMon = ?CHILD(dripline_conf_mon,worker),
    ConfMgr = ?CHILD(dripline_conf_mgr,worker),
    CmdMon  = ?CHILD(dripline_cmd_mon,worker),
    Dispatcher = ?CHILD(dripline_dispatch,worker),
    DLogSup = ?CHILD(dripline_data_log_sup,supervisor),
    {ok, { SuperStrategy, [Compiler,
			   ConnMgr,
			   ConfMgr,
			   ConfMon,
			   CmdMon,
			   InstrSup,
			   DLogSup,
			   Dispatcher] }}.
