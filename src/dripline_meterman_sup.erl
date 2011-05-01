% dripline_meterman_sup.erl
% 
-module(dripline_meterman_sup).
-behavior(supervisor).

% supervisor exports
-export([start_link/0,init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Child = {dripline_meterman,
	     {dripline_meterman,start_link,[]},
	     transient,
	     1000,
	     worker,
	     [dripline_meterman]},
    {ok, {{simple_one_for_one,5,10},[Child]}}.
