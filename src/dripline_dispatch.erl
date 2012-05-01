%% @doc The dripline_dispatch module exports a single function, 
%%		dispatch/1, whose job it is to take documents from the
%%		couchdb changes feed (as processed by the cmd and conf
%%		monitors) and dispatch the processed result to the 
%%		appropriate actors.  The result is then sent back to the
%%		database.
%% @version 0.1a
%% @author Jared Kofron <jared.kofron@gmail.com>
-module(dripline_dispatch).
-behavior(gen_server).

%%%%%%%%%%%%%%%%%%%%
%%% Exported API %%%
%%%%%%%%%%%%%%%%%%%%
-export([dispatch/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal state records %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,{}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Exported API Definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc dispatch/1 accepts a change document, strips off the document
%%		itself, and essentially parses down the command into funs.  the
%%		funs are then called by processes that are spawned from within
%%		the dispatcher.
%% @end
%%---------------------------------------------------------------------%%
-spec dispatch(ejson:json_object(),string()) -> ok.
dispatch(DocUpdateLine, DBSource) ->
    gen_server:cast(?MODULE,{dispatch,DBSource, DocUpdateLine}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm api definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    InitialState = #state{},
    {ok, InitialState}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({dispatch, DB, ChangeLine}, State) ->
    DocID = props:get('doc._id',ChangeLine),
    Updater = fun(DlData) ->
		      R = dripline_data:get_data(DlData),
		      TS = dripline_data:get_ts(DlData),
		      Props = [{<<"result">>,R}, {<<"timestamp">>, TS}],
		      dripline_util:update_couch_doc(DB,DocID,Props)
	      end,
    ToDo = case dripline_compiler:compile(ChangeLine) of
	       {ok, F} ->
		   fun() ->
			   R = F(),
			   Updater(R)
		   end;
	       Error ->
		   fun() ->
			   Updater(dripline_error:to_json(Error))
		   end
	   end,
    spawn(ToDo),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal function definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
