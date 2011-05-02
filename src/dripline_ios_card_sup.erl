% dripline_ios_card_sup.erl
% 
-module(dripline_ios_card_sup).
-behavior(supervisor).

% supervisor exports
-export([start_link/0,init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Child = {dripline_ios_card,
	     {dripline_ios_card,start_link,[]},
	     permanent,
	     brutal_kill,
	     worker,
	     [dripline_meterman]},
    erlang:spawn(fun() -> spawn_agent() end),
    {ok, {{simple_one_for_one,5,10},[Child]}}.

spawn_agent() ->
    timer:sleep(1000),
    Cards = dripline_backplane:scan(),
    lists:foreach(fun({_Slot,_Model}=Spec) ->
			  S = erlang:tuple_to_list(Spec),
			  {ok,_P} = supervisor:start_child(?MODULE,S) end,
		  Cards).
