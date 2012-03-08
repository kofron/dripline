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

handle_event(_Event, _StateName, StateData) ->
	{stop, unexpected_event, StateData}.

handle_sync_event(_Event,_From,_StateName,StateData) ->
	{stop, unexpected_sync_event, StateData}.

handle_info(_Info, _StateName, StateData) ->
	{stop, unexpected_info, StateData}.

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
	{Time, _} = timer:tc(fun() -> write_couch_spec(R) end),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%---------------------------------------------------------------------%%
%% @doc write_couch_spec/1 takes a binary value as returned by an 
%%		instrument and pushes it to the couchdb backend as a new 
%%		document.
%% @end
%%---------------------------------------------------------------------%%
-spec write_couch_spec(binary()) -> {ok, binary()} | {error, term()}.
write_couch_spec(Data) ->
	% create the new document
	NewDoc = {[]},
	D0 = couchbeam_doc:set_value("data",Data,NewDoc),
	% OK, get a handle to the database and write it.
	SConn = dripline_conn_mgr:get(),
	{ok, Db} = couchbeam:open_or_create_db(SConn,"dripline_logged_data"),
	couchbeam:save_doc(Db,D0).