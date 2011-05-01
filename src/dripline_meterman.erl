% dripline_meterman.erl
% written by jared kofron <jared.kofron@gmail.com>
% a state machine which is dedicated to reading a particular card's data
% and committing the result to the postgreSQL backend.
-module(dripline_meterman).
-behavior(gen_fsm).

% gen_fsm exports
-export([start_link/1,init/1,terminate/3,code_change/4]).
-export([handle_info/3,handle_event/3,handle_sync_event/4]).

start_link(SlotName) ->
    gen_fsm:start_link({local, ?MODULE},?MODULE,[SlotName],[]).

init([SlotName]) ->
    io:format("got slot name ~p~n",[SlotName]),
    ok.

terminate(_Reason,_StateName,_StateData) ->
    ok.

handle_event(_Event,StateName,StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event(_Event,_From,StateName,StateData) ->
    {reply, ok, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

code_change(_OldVsn,StateName,StateData,_Extra) ->
    {ok,StateName,StateData}.
