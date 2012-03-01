-module(agilent34970a).
-behavior(gen_server).

%% API
-export([start_link/3]).
-export([read/2]).

%% DEBUG
-export([channel_tuple_list_to_channel_spec/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(req_data, {from, ref}).
-record(state, {id, gpib_addr, epro_handle, c_req}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(InstrumentID,PrologixID,BusAddress) ->
  gen_server:start_link({local, InstrumentID}, ?MODULE, [InstrumentID,PrologixID,BusAddress], []).


%%--------------------------------------------------------------------
%% Function: read(Instrument, ChannelSpec) -> 
%%										{ok, Result} | {error, Error}
%% Description: Asks for data from the device by querying the prologix
%% handle.  Returns error codes (if generated) from the device itself
%%--------------------------------------------------------------------
read(InstrumentID,ChannelSpec) ->
	gen_server:call(InstrumentID,{read,ChannelSpec}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([InstrumentID,PrologixID,BusAddress]) ->
	{ok, #state{gpib_addr=BusAddress,id=InstrumentID,epro_handle=PrologixID}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({read,Channels}, From, #state{epro_handle = H}=StateData) ->
	ReadStr = read_channel_string(Channels),
	AddrStr = "++addr 9\n",
	{ok, R} = eprologix_cmdr:send_query(H,[AddrStr|ReadStr]),
	OutgoingReq = #req_data{from=From,ref = R},
	NewStateData = StateData#state{c_req = OutgoingReq},
	{noreply, NewStateData}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({R,Data}, #state{c_req=#req_data{from=F,ref=R}}=StateData) ->
	gen_server:reply(F,Data),
	NewStateData = StateData#state{c_req=none},
	{noreply, NewStateData}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: channel_tuple_to_int({integer(),integer()}) -> integer()
%% Description: Converts from "human syntax", which is to say {card,
%% channel}, to the syntax that the 34970 likes for channel 
%% designations.
%%--------------------------------------------------------------------
channel_tuple_to_int({CardNumber,ChannelNumber}) ->
	100*CardNumber + ChannelNumber.

channel_tuple_list_to_channel_spec([]) ->
	"";
channel_tuple_list_to_channel_spec(Tuples) ->
	IntChannels = [channel_tuple_to_int(Y) || Y <- Tuples],
	channel_ints_to_channel_spec(lists:sort(IntChannels),false,[]).

channel_ints_to_channel_spec([], false, Acc) ->
	lists:reverse(Acc);
channel_ints_to_channel_spec([], H0, Acc) ->
	Str = io_lib:format(":~p",[H0]), 
	lists:reverse([Str|Acc]);

channel_ints_to_channel_spec([H1,H2|T],false,Acc) when H2 == (H1 + 1) ->
	Str = io_lib:format("~p",[H1]),
	channel_ints_to_channel_spec(T,H2,[Str|Acc]);

channel_ints_to_channel_spec([H|T],H0,Acc) when H == (H0 + 1) ->
	channel_ints_to_channel_spec(T,H,Acc);

channel_ints_to_channel_spec([S],false,Acc) ->
	Str = io_lib:format("~p",[S]),
	channel_ints_to_channel_spec([],false,[Str|Acc]);

channel_ints_to_channel_spec([H|T],false,Acc) ->
	Str = io_lib:format("~p",[H]),
	channel_ints_to_channel_spec(T,false,[Str ++ ","|Acc]);

channel_ints_to_channel_spec([_H|_T]=L,H0,Acc) ->
	Str = io_lib:format(":~p",[H0]),
	channel_ints_to_channel_spec(L,false,[Str ++ ","|Acc]).

read_channel_string(ChannelList) ->
	ChannelSpec = channel_tuple_list_to_channel_spec(ChannelList),
	"MEAS:VOLT:DC? 10,0.003, (@" ++ ChannelSpec ++ ")".
