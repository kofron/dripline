%% @doc this module is the "application level" API module for 
%%		dripline.  it exports and does absolutely nothing so
%%		far.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
%% @todo we should probably move some stuff to this module that
%%		is used application-wide.
-module(dripline).


-export([start_logging/3,stop_logging/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API implementation %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc start_logging/3 starts a new data logging process on a target 
%%		channel with some interval and some maximum number of read cycles
%%		before death.
%% @end
%%---------------------------------------------------------------------%%
-spec start_logging(binary(),integer(),integer() | infinity) 
		-> ok | {error,term()}.
start_logging(ChannelName,Interval,infinity) ->
	case dripline_conf_mgr:get_logger_pid(ChannelName) of
		{ok, _P} ->
			{error, already_logging};
		{error, no_logger} ->
			StArg = [ChannelName,Interval,infinity],
			{ok,P} = supervisor:start_child(dripline_data_log_sup,StArg),
			dripline_conf_mgr:set_logger_pid(ChannelName,P)
	end;
start_logging(ChannelName,Interval,MaxIt) ->
	StartArgs = [ChannelName,Interval,MaxIt],
	supervisor:start_child(dripline_data_log_sup,StartArgs).

%%---------------------------------------------------------------------%%
%% @doc stop_logging/1 attempts to gracefully terminate the process 
%%		associated with logging the channel named by the argument by 
%%		sending a message to it indicating it's time to stop.
%% @end
%%---------------------------------------------------------------------%%
-spec stop_logging(binary()) -> ok | {error, no_logger}.
stop_logging(ChannelName) ->
	case dripline_conf_mgr:get_logger_pid(ChannelName) of
		{ok, P} ->
			P ! {stop, user_request};
		{error, no_logger}=E ->
			E
	end.