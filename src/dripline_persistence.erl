% dripline_persistence.erl
% written by jared kofron <jared.kofron@gmail.com>
% wraps all persistence abilities of dripline.  essentially what happens:
% when a new data point gets pushed to this module, enqueue it unless the
% maximum number of enqueued rows has been exceeded.  if it has, try to
% push the data to postgres.  if that fails, persist the data to dets.
% if it succeeds, increase the dets 'throttle' and go back to the phase
% where we read from dets.
% the logic comes from the case (hopefully rare) where the data is backing
% up due to a failed connection to pgsql.  once we get a connection we 
% want to get back to the 'empty dets' scenario as fast as possible.
-module(dripline_persistence).
-behavior(gen_fsm).

% internal state record
-record(state,{dets_table}).

% API
-export([enqueue/1]).

% states!
-export([idle/2]).
%-export([dets_scan/2,dets_read/2,dets_write/2]).
%-export([pg_write/2]).

% definitions
-define(immediately,0).

% gen_fsm exports
-export([start_link/0,init/1,terminate/3,code_change/4]).
-export([handle_info/3,handle_event/3,handle_sync_event/4]).

% API
enqueue(DataPoint) ->
    ok.

% states
idle(_Event,StateData) ->
    {next_state, idle, StateData}.

% gen_fsm defs
start_link() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE,[],[]).

init([]) ->
    {ok,idle,#state{},?immediately}.

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
