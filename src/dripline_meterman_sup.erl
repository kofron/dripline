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
	     permanent,
	     brutal_kill,
	     worker,
	     [dripline_meterman]},
    erlang:spawn(fun() -> spawn_agent() end),
    {ok, {{simple_one_for_one,5,10},[Child]}}.

spawn_agent() ->
    timer:sleep(1000),
    Cards = dripline_backplane:scan(),
    lists:foreach(fun({Slot,Model}) ->
			  io:format("~p/~p~n",[Slot,Model]),
			  Args = [Slot,Model],
			  supervisor:start_child(?MODULE,Args) end,
		  Cards).
