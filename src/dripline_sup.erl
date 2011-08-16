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
    application:start(nprocreg),
    application:load(mochiweb),
    BindAddress = "0.0.0.0",
    Port = 8000,
    ServerName = nitrogen,
%    DocRoot = "./site/static",
    
    io:format("attempting to start mochiweb...\n"),
    
    MochiOpts = [
		 {name, ServerName},
		 {ip, BindAddress},
		 {port, Port},
		 {loop, fun dripline_mochiweb:loop/1}
		],
    mochiweb_http:start(MochiOpts),

    {ok, { {one_for_one, 5, 10}, []} }.

