% dripline_ios_card.erl
% written by jared kofron <jared.kofron@gmail.com>
% a state machine which acts as a wrapper around a particular
% hardware card.  requests to go to cardA, cardB, etc are routed
% through this state machine.
-module(dripline_ios_card).
-behavior(gen_fsm).

% internal state record
-record(state,{slot,model}).

% states!
-export([ready/2,ready/3]).

% gen_fsm exports
-export([start_link/2,init/1,terminate/3,code_change/4]).
-export([handle_info/3,handle_event/3,handle_sync_event/4]).

% states
ready(read, _From, #state{slot=S}=StateData) ->
    T = erlang:now(),
    D = fake_data(),
    {reply, pack_ios_data(T,D,S), ready, StateData};
ready(_Event, _From, StateData) ->
    {reply, ok, ready, StateData}.
ready(_Event, StateData) ->
    {next_state, ready, StateData}.

% internal
fake_data() ->
    fake_data(20,[]).
fake_data(N,Acc) when N >= 0 ->
    fake_data(N-1,Acc ++ [{N,0.0}]);
fake_data(_,Acc) ->
    Acc.
pack_ios_data({MS,S,US}=_T,D,Slot) ->
    EpochSecs = erlang:round(1000000*MS + S + 0.000001*US),
    tag_ios_data([{timestamp,EpochSecs},{data,D}],Slot).
tag_ios_data(D,Slot) ->
    D ++ [{card,Slot}].

% gen_fsm exports
start_link(SlotName,CardModel) ->
    gen_fsm:start_link({local,SlotName},?MODULE,[SlotName,CardModel],[]).

init([SlotName,CardModel]) ->
    {ok,ready,#state{slot=SlotName,model=CardModel}}.

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
