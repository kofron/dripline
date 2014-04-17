%%%-------------------------------------------------------------------
%%% @author Jared Kofron <jared.kofron@gmail.com>
%%% @copyright (C) 2014, Jared Kofron
%%% @doc
%%%
%%% @end
%%% Created : 10 Feb 2014 by Jared Kofron <jared.kofron@gmail.com>
%%%
%%% Ways in which this could be improved:
%%% -Currently polling intervals are determined by least common 
%%% denominator.  It's hard-coded how quickly polling is done and
%%% that's dumb, as it limits the speed of the interface to the speed
%%% of the slowest instrument.  Each instrument could in principle be
%%% given control over the speed of its loop, and probably should.
%%% -There is a hidden state, "handle_status_byte", which needs to be
%%% unhidden to avoid a potential bug.
%%%-------------------------------------------------------------------
-module(gen_gpib_spoller).
-behaviour(gen_fsm).

%% API
-export([start_link/5,
	 get/2,
	 set/3,
	 send_sync/2]).

%% gen_fsm callbacks
-export([init/1,handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% states
-export([polling/2,
	 polling/3,
	 expect_response/2,
	 expect_response/3,
	 fetch_status_byte/2,
	 fetch_status_byte/3,
	 clear_status_byte/2,
	 clear_status_byte/3]).

-export([behaviour_info/1]).

-define(SERVER, ?MODULE).
-define(POLL_INTERVAL, 300). %% polling interval in milliseconds
-define(IMMEDIATE, 300). %% 
-define(WAIT, 1000).

-record(state, {
	  req_queue,
	  req_status,
	  bus_mod,
	  bus_name,
	  instr_state,
	  instr_name,
	  instr_mod,
	  instr_addr
	 }).

%% behavior
behaviour_info(callbacks) ->
    [{init,1}, 
     {sre_register_bitmask,1},
     {ese_register_bitmask,1},
     {handle_stb, 2},
     {handle_esr, 2},
     {handle_get, 2}];
behaviour_info(_) ->
    undefined.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the state machine associated with a gpib controller.
%% 
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(InstrMod, InstrName, InstrAddr, BusMod, BusName) ->
    gen_fsm:start_link({local, InstrName}, 
		       ?MODULE, 
		       [InstrMod, InstrName, InstrAddr, BusMod, BusName], 
		       []).

%%--------------------------------------------------------------------
%% @doc
%% send an iolist to a particular gpib address via the controller 
%% synchronously.
%% 
%% @spec send_sync(integer(), iolist()) -> 
%%         {ok, binary()} | 
%%         {device_error, binary()} | 
%%         {error, term() | 
%%         timeout
%% @end
%%--------------------------------------------------------------------
-spec send_sync(integer(), iolist()) -> 
		       {ok, binary()} | 
		       {device_error, binary()} | 
		       {error, term()} |	
		       timeout.
send_sync(InstrName, Data) ->
    gen_fsm:sync_send_all_state_event(InstrName,
				      {send, Data}).

%%--------------------------------------------------------------------
%% @doc
%% get is called when data is to be retrieved from an instrument.
%% 
%% @spec get(atom(), atom()) -> {ok, dl_data:dl_data()} | {error, term()}.
%% @end
%%--------------------------------------------------------------------
-spec get(atom(), atom()) -> dl_data:dl_data().
get(InstrumentName, ChannelName) ->
    gen_fsm:sync_send_all_state_event(InstrumentName,
				      {get, ChannelName}).

%%--------------------------------------------------------------------
%% @doc
%% set is called when data is to be sent to an instrument and asserted
%% on some channel.
%% 
%% @spec set(atom(), atom(), term()) -> 
%%   {ok, dl_data:dl_data()} | {error, term()}.
%% @end
%%--------------------------------------------------------------------
-spec set(atom(), atom(), term()) -> dl_data:dl_data().
set(InstrumentName, ChannelName, Value) ->
    gen_fsm:sync_send_all_state_event(InstrumentName,
				      {set, ChannelName, Value}).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([InstrMod, _InstrName, _InstrAddr, _BusMod, _BusName]=Args) ->
    case InstrMod:init([]) of
	{ok, InstrState} ->
	    normal_init(Args, InstrState);
	AnyOther ->
	    AnyOther
    end.
normal_init([InstrMod, InstrName, InstrAddr, BusMod, BusName], InstrState) ->
    ok = setup_sre_registers(BusMod, BusName, InstrMod, InstrAddr),
    {ok, 
     polling, 
     #state{
	req_queue=[],
	req_status=ok,
	bus_name=BusName,
	bus_mod=BusMod,
	instr_state=InstrState,
	instr_name=InstrName,
	instr_mod=InstrMod,
	instr_addr=InstrAddr
       },
     0}.
setup_sre_registers(BusMod, BusName, InstrMod, InstrAddr) ->
    SREMask = InstrMod:sre_register_bitmask(none),    
    ok = BusMod:send(BusName, 
		     InstrAddr,
		     [<<"*sre ">>, erlang:integer_to_binary(SREMask)]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% polling means that the gpib controller is trolling for a non-zero
%% status byte on an instrument.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
polling(timeout, #state{bus_mod=M, bus_name=N, instr_addr=A}=SD) ->
    ok = M:serial_poll(N, A),
    {next_state, polling, SD, ?POLL_INTERVAL}.
polling(_Event, _From, StateData) ->
    {stop, unimplemented, StateData}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% expect_response means MAV was set, and we sent a request to have the
%% output buffer sent to us.  timing out here means it did not arrive
%% in a timely manner.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
expect_response(timeout, #state{req_queue=[]}=SD) ->
    {next_state, polling, SD, ?IMMEDIATE};
expect_response(timeout, #state{req_queue=[{_, Fr}|W]}=StateData) ->
    gen_fsm:reply(Fr, {error, timed_out_waiting_for_instrument}),
    {next_state, polling, StateData#state{req_queue=W}, ?IMMEDIATE}.
expect_response(_Event, _From, SD) ->
    {stop, unimplemented, SD}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% clear_status_byte is intended to clear the ESR register after a
%% successful operation so that the instrument is ready again.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
clear_status_byte(timeout, StateData) ->
    {next_state, polling, StateData, ?IMMEDIATE}.
clear_status_byte(_Event, _From, SD) ->
    {stop, unimplemented, SD}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% fetch_status_byte is intended to fetch the ESR register for the 
%% callback module to respond to.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
fetch_status_byte(timeout, #state{bus_mod=M,
				  bus_name=N,
				  instr_addr=A}=StateData) ->
    ok = M:serial_poll(N, A),
    {next_state, fetch_status_byte, StateData, ?IMMEDIATE}.
fetch_status_byte(_Event, _From, SD) ->
    {stop, unimplemented, SD}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event({send, Dt}, Fr, 
		  State, #state{req_queue=Q,
				bus_mod=M,
				bus_name=S,
				instr_addr=Addr}=SD) ->
    {ToSend, Req} = case is_query(Dt) of
			true ->
			    {Dt, {qry, Fr}};
			false ->
			    %% actually need to strip whitespace and so on here
			    %% to make sure this is a correctly formed query.
			    {[Dt,<<";*OPC">>], {stmt, Fr}}
		    end,
    NewQueue = Q ++ [Req],
    ok = M:send(S,Addr,ToSend),
    {next_state, State, SD#state{req_queue=NewQueue}, ?IMMEDIATE};
handle_sync_event({get, Ch}, Fr, StateName, 
		  #state{
		     instr_mod=I,
		     instr_state=St
		    }=SD) ->
    case I:handle_get(Ch, St) of
	{send, ToSend, NewStateData} ->
	    handle_sync_event({send, ToSend}, 
			      Fr, 
			      StateName, 
			      SD#state{instr_state=NewStateData});
	{error, Reason, NewStateData} ->
	    Reply = make_error_response(Reason),
	    gen_fsm:reply(Fr, Reply),
	    {next_state, StateName, SD#state{instr_state=NewStateData}, ?IMMEDIATE}
    end;
handle_sync_event({set, Ch, Val}, Fr, StateName,
		  #state{
		     instr_mod=I,
		     instr_state=St
		    }=SD) ->
    case I:handle_set(Ch, Val, St) of
	{send, ToSend, NewStateData} ->
	    handle_sync_event({send, ToSend},
			      Fr,
			      StateName,
			      SD#state{instr_state=NewStateData});
	{error, Reason, NewStateData} ->
	    Reply = make_error_response(Reason),
	    gen_fsm:reply(Fr, Reply),
	    {next_state, StateName, SD#state{instr_state=NewStateData}, ?IMMEDIATE}
    end.

is_query(Bin) when is_binary(Bin) ->
    case binary:match(Bin, <<$?>>) of
	nomatch ->
	    false;
	_AnyOther ->
	    true
    end;
is_query(BinList) when is_list(BinList) ->
    is_query(lists:last(BinList)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({gpib, B, Data}, polling,
	    #state{bus_name=B}=SD) ->
    %% convert the message to the value of the status byte register
    SBR = erlang:binary_to_integer(Data),

    %% if SRQ has been asserted, we need to consult the module about
    %% what to do.  this should get cleaned up so that the modules get
    %% everything, including the SRQ bit
    {NextState, NewSD} = case srq_asserted(SBR) of
			     false when SBR =:= 0 ->
				 {polling, SD};
			     false when SBR =/= 0, SBR > 16 ->
				 module_srq_feedback(SD, SBR);
			     false ->
				 {polling, SD};
			     true ->
				 module_srq_feedback(SD, SBR)
			 end,
    {next_state, NextState, NewSD, ?IMMEDIATE};
handle_info({gpib, N, Data}, fetch_status_byte,
	    #state{bus_name=N,
		   bus_mod=M,
		   instr_addr=A}=SD) ->
    %% convert the message to the value of the status byte register
    SBR = erlang:binary_to_integer(Data),

    %% if a message is available, that's the value of the ESR.  we want 
    %% to retrieve it.
    case SBR of
	80 ->
	    ok = M:read_eoi(N, A);
	Other ->
	    io:format("oh no we dont undertand ~p~n",[Other])
    end,
    {next_state, handle_status_byte, SD, ?WAIT};
%%% TODO: this state is hidden from the outside world (might be fine to do)
%%% but think about that!!
handle_info({gpib, BusName, Data}, handle_status_byte,
	    #state{bus_name=BusName,
		   bus_mod=BusMod,
		   instr_addr=InstrAddr,
		   instr_mod=I,
		   instr_state=IS,
		   req_queue=[{_, From}|Queue]}=SD) ->
    %% strip the terminator from the binary.
    StrippedESR = binary:part(Data, {0, erlang:byte_size(Data)-1}),
    ESR = erlang:binary_to_integer(StrippedESR),

    {NextState, NewSD} = case I:handle_esr(ESR, IS) of
			     {op_complete, NewState} ->
				 gen_fsm:reply(From, ok),
				 {polling, SD#state{req_queue=Queue,instr_state=NewState}};
			     {retrieve_error, ToSend, NewState} ->
				 ok = BusMod:send(BusName, InstrAddr, ToSend),
				 {polling, SD#state{req_status=error,instr_state=NewState}}
			 end,
    {next_state, NextState, NewSD, ?IMMEDIATE};
handle_info({gpib, B, Data}, 
	    expect_response,
	    #state{bus_name=B,
		   req_queue=[{Type, From}|Waiting],
		   req_status=Status}=SD) ->
    Reply = case Type of
		stmt when Status =:= error ->
		    make_error_response(Data);
		stmt when Status =:= ok ->
		    make_success_response(ok);
		qry ->
		    make_response(Status,Data)
	    end,
    gen_fsm:reply(From, Reply),
    {next_state, 
     polling, 
     SD#state{req_queue=Waiting,req_status=ok}, 
     ?POLL_INTERVAL};
handle_info({gpib, B, _Data},
	   expect_response,
	   #state{bus_name=B,
		 req_queue=[],
		 req_status=_Status}=SD) ->
    {next_state, 
     polling, 
     SD#state{req_status=ok}, 
     ?POLL_INTERVAL};
handle_info({gpib, B, _Data},
	    clear_status_byte,
	   #state{bus_name=B,
		  req_queue=[{_, From}|Waiting],
		  req_status=ok}=SD) ->
    gen_fsm:reply(From, ok),
    {next_state,
     polling,
     SD#state{req_queue=Waiting,req_status=ok},
     ?POLL_INTERVAL}.
srq_asserted(SRByte) ->
    ((SRByte bsr 6) band 1) == 1.
module_srq_feedback(#state{bus_mod=BusMod,
			   bus_name=BusName,
			   req_status=RS,
			   instr_addr=InstrAddr,
			   instr_mod=InstrMod, 
			   instr_state=S}=SD, SBR) ->
    case InstrMod:handle_stb(SBR, S) of
	{retrieve_error, ToSend, NewState} ->
	    ok = BusMod:send(BusName, InstrAddr, ToSend),
	    {polling, SD#state{req_status=error,instr_state=NewState}};
	{retrieve_data, NewState} ->
	    ok = BusMod:read_eoi(BusName, InstrAddr),
	    {expect_response, SD#state{req_status=RS,instr_state=NewState}};
	{fetch_esr, ToSend, NewState} ->
	    ok = BusMod:send(BusName, InstrAddr, ToSend),
	    {fetch_status_byte, SD#state{req_status=RS, instr_state=NewState}};
	{clear_esr, ToSend, NewState} ->
	    ok = BusMod:send(BusName, InstrAddr, ToSend),
	    {polling, SD#state{req_status=ok, instr_state=NewState}};
	{ignore, NewState} ->
	    {clear_status_byte, SD#state{instr_state=NewState}}
    end.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
make_response(error, Data) ->
    make_error_response(Data);
make_response(ok, Data) ->
    make_success_response(Data).
make_error_response(Data) ->
    D = dl_data:new(),
    D1 = dl_data:set_data(D, Data),
    D2 = dl_data:set_ts(D1, dl_util:make_ts()),
    dl_data:set_code(D2,error).
make_success_response(Data) ->
    D = dl_data:new(),
    D1 = dl_data:set_data(D, Data),
    D2 = dl_data:set_ts(D1, dl_util:make_ts()),
    dl_data:set_code(D2, ok).
