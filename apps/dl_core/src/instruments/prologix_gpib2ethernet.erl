%%%-------------------------------------------------------------------
%%% @author Jared Kofron <jared.kofron@gmail.com>
%%% @copyright (C) 2014, Jared Kofron
%%% @doc
%%% The prologix bus sends data over an prologix gpib to ethernet
%%% adapter.  Its job is to deal with control sequences and so on to
%%% transmit data to instruments. 
%%%
%%% @end
%%% Created : 12 Feb 2014 by Jared Kofron <jared.kofron@gmail.com>
%%%-------------------------------------------------------------------
-module(prologix_gpib2ethernet).
-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% api
-export([send/3, 
	 read_eoi/2,
	 serial_poll/2,
	 send_sync/3]).

%%% internal bus state
-record(state, {
	  bus_name,
	  waiting,
	  address,
	  port,
	  socket,
	  term_seq
	 }).

%%% convenience record to represent requests that are queued or sent.
-record(req, {
	  type,
	  from,
	  data,
	  timer
	 }).


%%%===================================================================
%%% API
%%%===================================================================
-spec read_eoi(atom(), integer()) -> ok.
read_eoi(PrologixName, Address) ->
    gen_server:cast(PrologixName, 
		    {send_sync, self(), Address, <<"++read eoi">>}).

-spec send(atom(), integer(), iolist()) -> ok.
send(PrologixName, Address, Data) ->
    gen_server:call(PrologixName, {send, Address, Data}).

-spec send_sync(atom(), integer(), iolist()) -> ok.
send_sync(PrologixName, Address, Data) ->
    gen_server:cast(PrologixName, {send_sync, self(), Address, Data}).

-spec serial_poll(atom(), integer()) -> ok.
serial_poll(PrologixName, Address) ->
    gen_server:cast(PrologixName,
		    {send_sync, self(), Address, <<"++spoll">>}).
			

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Address, Port, Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Address, Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name, Address, Port]) ->
    {ok, S} = gen_tcp:connect(Address, Port, [binary, {packet, 0}]),
    {ok, #state{bus_name=Name, 
		waiting=[],
		address=Address, 
		port=Port, 
		socket=S, 
		term_seq= <<$\r,$\n>>}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% set_address calls back into handle_call.  if a request is already in
%% progress, this will block until all pending request(s) have been 
%% processed.
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send, Address, Data},
	    From,
	    #state{socket=S,
		   waiting=W,
		   term_seq=T}=SD) ->
    Out = lists:flatten([<<"++addr ">>,
			 erlang:integer_to_binary(Address),
			 Data]),
    ToSend = pack_eprologix_iolist(Out,T),
    R = #req{type=send, from=From, data=ToSend},
    NewQueue = case W of
		   [] ->
		       gen_tcp:send(S, ToSend),
		       gen_server:reply(From, ok),
		       W;
		   _NonEmptyQueue ->
		       W ++ [R]
	       end,
    {noreply, SD#state{waiting=NewQueue}};
handle_call(_Call, _From, State) ->
    {stop, unimplemented, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send_sync, From, Address, Data}, 
	    #state{socket=S,
		   waiting=W,
		   term_seq=T}=SD) ->
    Out = lists:flatten([<<"++addr ">>,
			 erlang:integer_to_binary(Address),
			 Data]),
    ToSend = pack_eprologix_iolist(Out, T),
    R = #req{type=send_sync, from=From, data=ToSend},
    NewQueue = case W of
		       [] ->
			   gen_tcp:send(S, ToSend),
			   TRef = erlang:send_after(250, self(), reply_timeout),
			   [R#req{timer=TRef}];
		       _NonEmptyQueue ->
			   W ++ [R]
		   end,
    {noreply, SD#state{waiting=NewQueue}};

handle_cast(_Cast, State) ->
    {stop, unimplemented, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(reply_timeout,
	    #state{socket=S, waiting=[#req{type=send_sync,from=W}|Q]}=State) ->
    W ! reply_timed_out,
    {ok, NewQueue} = process_request_queue(S, Q),
    {noreply, State#state{waiting=NewQueue}};
handle_info({tcp, S, D}, 
	    #state{term_seq=T, bus_name=N, 
		   socket=S,
		   waiting=[#req{type=send_sync, from=W, timer=TR}|Q]}=SD) ->
    erlang:cancel_timer(TR),
    Res = strip_terminator(T, D),
    W ! {gpib, N, Res},
    {ok, NewQueue} = process_request_queue(S, Q),
    {noreply, SD#state{waiting=NewQueue}}.

strip_terminator(TermSeq, Data) ->
    case binary:match(Data, TermSeq) of
	nomatch ->
	    Data;
	{StartPos, _} -> 
	    binary:match(Data, TermSeq),
	    binary:part(Data, {0, StartPos})	
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
pack_eprologix_iolist(Data, T) ->
    case is_list(Data) of
	true ->
	    lists:foldl(fun(El, Acc) ->
				case binary:last(El) of
				    $\s ->
					Acc ++ [El];
				    _ ->
					Acc ++ [El] ++ [T]
				end
			end,
			[],
			Data);
	false ->
	    [Data, T]
    end.

process_request_queue(_Socket, []) ->
    {ok, []};
process_request_queue(Socket, [#req{type=send, 
				    from=From, 
				    data=ToSend}|Rest]) ->
    ok = gen_tcp:send(Socket, ToSend),
    gen_server:reply(From, ok),
    {ok, Rest};
process_request_queue(Socket, [#req{type=send_sync, 
				    data=ToSend}=R|Rest]) ->
    TRef = erlang:send_after(250, self(), reply_timeout),
    ok = gen_tcp:send(Socket, ToSend),
    {ok, [R#req{timer=TRef}|Rest]}.

