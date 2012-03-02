%% @doc dripline_data_logger is a workhorse module for dripline.  it 
%%		encapsulates a behavior that is used all over - take data, write
%%		the value to a database, wait some time, and repeat.  this is 
%%		used both in transient loggers and permanent loggers, and
%%		this module can do both.  the logger starts with a channel it
%%		is responsible for reading and an interval(in milliseconds), 
%%		generates an {M,F,A} with the help of dripline_conf_mon, and then 
%%		simply loops.  for the transient style logger, once the number of 
%%		iterations hits the approriate limit, it automatically dies.  
%%		in addition, for the transient logger, it writes to a *single* 
%%		document and repeatedly revises it.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
-module(dripline_data_logger).
-behavior(gen_fsm).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/4]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, 
		terminate/3, code_change/4]).

%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm states %%%
%%%%%%%%%%%%%%%%%%%%%%
-export([interrogating/2,writing/2,sleeping/2]).

%%%%%%%%%%%%%%%%%%%%%%
%%% internal state %%%
%%%%%%%%%%%%%%%%%%%%%%
-record(state,{call,interval,max_it,cur_it, elapsed, c_res, c_err}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Id,Call,Interval,MaxIterations) ->
	InitArgs = [Call,Interval,MaxIterations],
	gen_fsm:start_link({local,Id},?MODULE,InitArgs,[]).

init([Call,Interval,MaxIterations]) ->
	InitialState = #state{
		call = Call,
		interval = Interval,
		max_it = MaxIterations,
		cur_it = 0,
		elapsed = 0,
		c_res = none,
		c_err = false
	},
	{ok, interrogating, InitialState, 0}.

handle_event(Event, StateName, StateData) ->
	io:format("got event: ~p~n",[Event]),
	{next_state, StateName, StateData}.

handle_sync_event(Event,_From,StateName,StateData) ->
	io:format("got sync event:~p~n",[Event]),
	{reply, ok, StateName, StateData}.

handle_info(Info, StateName, StateData) ->
	io:format("got info:~p~n",[Info]),
	{next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
	ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm states %%%
%%%%%%%%%%%%%%%%%%%%%%
interrogating(timeout,#state{call=C}=StateData) ->
	Result = timer:tc(C),
	NewStateData = case Result of
		{_Elapsed,{error,_E}=Err} ->
			StateData#state{c_res = Err, c_err = true};
		{Elapsed,Data} ->
			StateData#state{c_res = Data, elapsed = Elapsed}
	end,
	{next_state, writing, NewStateData, 0}.

writing(timeout,#state{c_res=R,elapsed=T}=StateData) ->
	{Time, _} = timer:tc(fun() -> R end),
	NewStateData = StateData#state{
		c_res = none,
		elapsed = T + Time
	},
	{next_state, sleeping, NewStateData, 0}.

sleeping(timeout,#state{elapsed=E,cur_it=C,max_it=M,interval=I}=SData) ->
	Branch = case (C + 1) >= M of
		true -> % we've outstayed our welcome
			{stop, max_iterations_reached, SData};
		false -> % keep going, sleep for the right amount of time.
			SleepTime = calc_sleep_time(E,I),
			NewStateData = SData#state{
				cur_it = C + 1,
				elapsed = 0
			},
			{next_state, interrogating, NewStateData, SleepTime}
	end,
	Branch.

%%---------------------------------------------------------------------%%
%% @doc calc_sleep_time/2 is a utility function that is unfortunately 
%%		necessary due to the mix of units that we have to deal with.  the
%%		interval is in seconds, the elapsed time is in microseconds, and
%%		the sleep time is in milliseconds.
%% @end
%%---------------------------------------------------------------------%%
-spec calc_sleep_time(integer(),integer()) -> integer().
calc_sleep_time(ElapsedTime, IntervalTime) ->
	erlang:round(((IntervalTime*1000000) - ElapsedTime)/1000).