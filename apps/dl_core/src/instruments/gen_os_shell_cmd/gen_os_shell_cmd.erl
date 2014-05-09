%%%-------------------------------------------------------------------
%%% @author Ben LaRoque
%%%
%%% Created: 9 May 2014 by Ben Laroque
% gen_os_shell_cmd.erl
% Behavior for "instruments" which are really operating system
% commands.  The behavior module wraps a gen_server, and the
% callback module defines its 'base' command, and a list of
% valid options.  When the execute command is sent in, the base
% command along with the options that are passed in is executed
% via os:cmd.
-module(gen_os_shell_cmd).
-behaviour(gen_server).

%-export([behaviour_info/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([get/2,
         set/3]).
%%--------------------------------------------------------------------
%% gen_server
%%--------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%--------------------------------------------------------------------
%% API Functions
%%--------------------------------------------------------------------
get(Instrument, Channel) ->
    gen_server:call(Instrument, {get, Channel}).

set(Instrument, Channel, Value) ->
    gen_server:call(Instrument, {set, Channel, Value}).

%%--------------------------------------------------------------------
%% gen_server Functions
%%--------------------------------------------------------------------
init(_Args) ->
    ignore.

handle_call({get, _Channel}, _From, State) ->
    {reply, ok, State};
handle_call({set, _Channel, _Value}, _From, State) ->
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {stop, unrecognized_request, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {reply, State}.

terminate(_Request, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% helper Functions
%%--------------------------------------------------------------------
