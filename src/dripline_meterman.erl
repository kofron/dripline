% dripline_meterman.erl
% written by jared kofron <jared.kofron@gmail.com>
% a state machine which is dedicated to reading a particular card's data
% and committing the result to the postgreSQL backend.
-module(dripline_meterman).
-behavior(gen_fsm).

% internal state record
-record(state,{read_f :: fun(),
	       write_f :: fun(),
	       cycle_time = 1000 :: integer(),
	       attached_data = [] :: term()}).

% states!
-export([idle/2]).
-export([read/2]).
-export([write/2]).

% definitions
-define(immediately,0).

% gen_fsm exports
-export([start_link/2,init/1,terminate/3,code_change/4]).
-export([handle_info/3,handle_event/3,handle_sync_event/4]).

% state
idle(timeout, #state{cycle_time=C}=StateData) when C > 0 ->
    {next_state, read, StateData#state{cycle_time=1000}, C};
idle(timeout, StateData) ->
    {next_state, read, StateData#state{cycle_time=1000}, ?immediately}.
read(timeout, #state{read_f=F}=StateData) ->
    {Data,DT} = time_execution(F),
    StateP = attach_data(StateData,Data),
    {next_state, write, decrement_cycle_time(StateP,DT), ?immediately}.
write(timeout, #state{write_f=W,attached_data=D}=StateData) ->
    {_,DT} = time_execution(W,D),
    StateP = detach_data(StateData),
    {next_state, idle, decrement_cycle_time(StateP,DT), ?immediately}.

% gen_fsm exports
start_link(SlotName,CardModel) ->
    gen_fsm:start_link(?MODULE,[SlotName,CardModel],[]).

init([SlotName,_CardModel]) ->
    FRead = fun() -> dripline:read(SlotName) end,
    FWrite = fun(X) -> dripline_persistence:enqueue(X) end,
    {ok,idle,#state{read_f = FRead,write_f = FWrite},?immediately}.

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

% internal
attach_data(State,D) when is_record(State, state) ->
    State#state{attached_data = D}.

detach_data(State) when is_record(State, state) ->
    State#state{attached_data = []}.

decrement_cycle_time(#state{cycle_time=C}=S,DT) ->
    S#state{cycle_time = C - DT}.

time_execution(F) ->
    T0 = erlang:now(),
    D = F(),
    DT = erlang:round(timer:now_diff(erlang:now(),T0)/1000),
    {D,DT}.
time_execution(F,A) ->
    T0 = erlang:now(),
    D = F(A),
    DT = erlang:round(timer:now_diff(erlang:now(),T0)/1000),
    {D,DT}.    
