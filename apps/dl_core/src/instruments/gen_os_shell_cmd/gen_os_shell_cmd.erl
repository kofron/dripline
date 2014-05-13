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
-export([start_link/2,
         get/2,
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
start_link(CallbackMod, ID) ->
    lager:notice("my ID is:~p",[ID]),
    Res = gen_server:start_link({local, ID}, ?MODULE, [CallbackMod], []),
    lager:notice("new vsn start_link result is: ~p", [Res]),
    Res.

get(Instrument, Channel) ->
    lager:notice("the instruments are:~n~p", [supervisor:which_children(dl_instr_sup)]),
    lager:notice("and I am:~p", [self()]),
    lager:notice("an os_shell get ch:(~p) on instr:(~p)", [Channel, Instrument]),
    gen_server:call(Instrument, {get, Channel}).

set(Instrument, Channel, Value) ->
    lager:notice("an os_shell set ch:(~p) on instr:(~p) to val:(~p)", [Channel, Instrument, Value]),
    gen_server:call(Instrument, {set, Channel, Value}).

%%--------------------------------------------------------------------
%% gen_server Functions
%%--------------------------------------------------------------------
init(_Args) ->
    ignore.

handle_call({get, _Channel}, _From, State) ->
    lager:notice("os_shell get"),
    {reply, ok, State};
handle_call({set, _Channel, _Value}, _From, State) ->
    lager:notice("os_shell set"),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    lager:warning("os_shell unknown request"),
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
