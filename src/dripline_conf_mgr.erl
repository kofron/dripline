%% @doc dripline_conf_mgr is one of the core dripline 'managers' and is
%%		probably the most important module in the entire codebase.  it 
%%		stores all of the state regarding the current configuration, and
%%		is able to generate executable fun()s from that state.  whenever
%%		a query or command is sent through the database, calls are made
%%		to this module in order to determine what to do.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a
%% @todo clean this up for god's sake
%% @todo memoizing state would increase efficiency
-module(dripline_conf_mgr).
-behavior(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([add_channel/1,lookup/1,all_channels/0]).
-export([add_instr/1,lookup_instr/1,all_instr/0]).
-export([get_logger_pid/1,set_logger_pid/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state record %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{
			chs, % Channel data
			ins, % Instrument data
			lgs  % Logger data
		}).

%%%%%%%%%%%%%%
%%% Macros %%%
%%%%%%%%%%%%%%
-define(NOW, 0).

%%%%%%%%%%%%%%%%%%%%%%
%%% API definition %%%
%%%%%%%%%%%%%%%%%%%%%%
-spec add_channel(term()) -> ok | {error,already_exists}.
add_channel(ChannelData) ->
	gen_server:call(?MODULE,{add_channel, ChannelData}).

-spec lookup(binary()) -> {ok,term()} | {error,term()}.
lookup(ChName) ->
	gen_server:call(?MODULE,{lookup,{ch,ChName}}).

-spec all_channels() -> list().
all_channels() ->
	gen_server:call(?MODULE,all_channels).

-spec get_logger_pid(binary()) -> {ok,pid()} | {error,no_logger}.
get_logger_pid(ChannelName) ->
	gen_server:call(?MODULE,{get_lg_pid, ChannelName}).

-spec set_logger_pid(binary(),pid()) -> ok.
set_logger_pid(ChannelName, Pid) ->
	gen_server:call(?MODULE,{set_lg_pid, ChannelName, Pid}).	

-spec add_instr(dripline_instr_data:instr_data()) -> ok.
add_instr(Data) ->
	gen_server:call(?MODULE,{add_instr, Data}).

-spec lookup_instr(binary()) -> 
	{ok,dripline_instr_data:instr_data()} | 
	{error, {bad_instr,binary()}}.
lookup_instr(InstrName) ->
	gen_server:call(?MODULE,{lookup,{in,InstrName}}).

-spec all_instr() -> [dripline_instr_data:instr_data()].
all_instr() ->		    
    gen_server:call(?MODULE,all_instr).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback defs %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init([]) ->
	InitialState = #state{
		chs = dict:new(),
		lgs = dict:new(),
		ins = dict:new()
	},
	{ok, InitialState, ?NOW}.

handle_call({add_channel,Data}, _F, #state{chs=Ch}=StateData) ->
	{ok, ChannelName} = dripline_ch_data:get_fields(id,Data),
	{NewStateData, Reply} = case dict:is_key(ChannelName,Ch) of
		true ->
			{StateData, {error, already_exists}};
		false ->
			TNewStateData = StateData#state{
				chs = dict:store(ChannelName,Data,Ch)
			},
			{TNewStateData, ok}
	end,
	{reply, Reply, NewStateData};
handle_call({add_instr,Data}, _F, #state{ins=In}=StateData) ->
	InstrName = dripline_instr_data:get_id(Data),
	NewStateData = StateData#state{
		ins = dict:store(InstrName,Data,In)
	},
	{reply, ok, NewStateData};
handle_call({lookup,{ch,Name}}, _From, #state{chs=Ch}=StateData) ->
	Reply = case dict:find(Name,Ch) of
		{ok, Value} ->
			{ok, Value};
		error ->
			{error, {bad_channel,Name}}
	end,
	{reply, Reply, StateData};
handle_call({lookup,{in,Name}}, _From, #state{ins=In}=StateData) ->
	Reply = case dict:find(Name,In) of
		{ok, Value} ->
			{ok, Value};
		error ->
			{error, {bad_instr,Name}}
	end,
	{reply, Reply, StateData};
handle_call(all_channels, _F, #state{chs=Ch}=StateData) ->
	Reply = dict:fetch_keys(Ch),
	{reply, Reply, StateData};
handle_call(all_instr, _F, #state{ins=In}=StateData) ->
	Reply = dict:fetch_keys(In),
	{reply, Reply, StateData};
handle_call({get_lg_pid, Name}, _F, #state{lgs=Lg}=StateData) ->
	Reply = case dict:find(Name,Lg) of
		{ok, _Pid}=C ->
			C;
		error ->
			{error, no_logger}
	end,
	{reply, Reply, StateData};
handle_call({set_lg_pid, Name, Pid}, _F, #state{lgs=Lg}=StateData) ->
	erlang:monitor(process,Pid),
	NewState = StateData#state{
		lgs = dict:store(Name,Pid,Lg)
	},
	{reply, ok, NewState}.

handle_cast(_Cast, StateData) ->
	{noreply, StateData}.

handle_info({'DOWN',_Ref, process, Pid, Reason}, #state{lgs=Lg}=SData) ->
	NewStateData = case Reason of
		normal -> %% need to drop this pid from dictionary
			SData#state{
				lgs = drop_pid(Lg,Pid)
			};
		shutdown -> %% need to drop this pid from dictionary
			SData#state{
				lgs = drop_pid(Lg,Pid)
			};
		_Other -> %% Should we try to track down the new process?
			SData
	end,
	{noreply, NewStateData};
handle_info(timeout, StateData) ->
	spawn(dripline_jumpstart,go,[]),
	{noreply, StateData};
handle_info(_Info,StateData) ->
	{stop, unexpected_info, StateData}.

terminate(_Reason, _StateData) ->
	ok.

code_change(_OldVsn, StateData, _Extra) ->
	{ok, StateData}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal Functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
drop_pid(LogDict,Pid) ->
	Search = fun(X) -> fun(_K,_V,{true,_D2}=NoOp) -> 
							NoOp;
				 		(K,X,{false,D1}) ->
				 			{true, dict:erase(K,D1)}
						end
	end,
	{true, D2} = dict:fold(Search(Pid),{false,LogDict},LogDict),
	D2.
