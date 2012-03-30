-module(dripline_compiler).
-behaviour(gen_server).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([compile/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server api and callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal server state %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state, {}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% intermediate data record %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(intermed, {type, do, channel, value}).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec compile(ejson:json_object()) -> 
		     {ok, fun()} | {error, dripline_error:error()}.
compile(JSON) ->
    gen_server:call(?MODULE,{compile, JSON}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server API and callback definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call({compile, JS}, _From, State) ->
    Reply = case drip_compile(JS) of
		{ok, _F}=Success ->
		    Success;
		{error, _E}=Err ->
		    Err
	    end,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec drip_compile(ejson:json_object()) -> 
			  {ok, fun()} | {error, dripline_error:error()}.
drip_compile(JS) ->
    New = #intermed{},
    case resolve_type(JS,New) of
	{ok, Typed} ->
	    resolve_action(JS,Typed);
	{error, notype} ->
	    dripline_error:compiler_expected(type_field,no_type_field);
	{error, {bad_type, T}} ->
	    dripline_error:compiler_surprised(<<"type">>,T)
    end.

-spec resolve_type(ejson:ejson_object(), #intermed{}) ->
			  {ok, binary()} | {error, notype}.
resolve_type(JS, Inter) ->
    case couchbeam_doc:get_value(<<"type">>,JS) of
	undefined ->
	    {error, notype};
	Type ->
	    case lists:member(Type, type_tokens()) of
		true ->
		    AType = dripline_util:binary_to_atom(Type),
		    {ok, Inter#intermed{type=AType}};
		false ->
		    {error, {bad_type, Type}}
	    end
    end.

-spec type_tokens() -> [binary()].
type_tokens() ->
    [
     <<"command">>,
     <<"system">>,
     <<"channel">>,
     <<"instrument">>
    ].

-spec resolve_action(ejson:ejson_object(), #intermed{}) ->
			    {ok, #intermed{}} | dripline_error:error().
resolve_action(JS,#intermed{type=command}=I) ->
    case couchbeam_doc:get_value(<<"command">>,JS) of
	undefined ->
	    dripline_error:field_undefined(compiler,command);
	Cmd ->
	    case couchbeam_doc:get_value(<<"do">>,Cmd) of
		undefined ->
		    dripline_error:field_undefined(compiler,do);
		Do ->
		    case lists:member(Do,action_tokens()) of
			true ->
			    ADo = dripline_util:binary_to_atom(Do),
			    resolve_target(Cmd,I#intermed{do=ADo});
			false ->
			    dripline_error:compiler_surprised(do,Do)
		    end
	    end
    end.

-spec action_tokens() -> [binary()].
action_tokens() ->
    [
     <<"get">>,
     <<"set">>
    ].

-spec resolve_target(ejson:json_object(),#intermed{}) -> 
			    {ok, #intermed{}} | dripline_error:error().
resolve_target(JS,#intermed{type=command,do=get}=I) ->
    io:format("~p~n",[JS]),
    case couchbeam_doc:get_value(<<"channel">>,JS) of
	undefined ->
	    dripline_error:field_undefined(compiler,channel);
	Ch ->
	    {ok, I#intermed{channel=Ch}}
    end;
resolve_target(JS,#intermed{type=command,do=set}=I) ->	
    case couchbeam_doc:get_value(<<"channel">>,JS) of
	undefined ->
	    dripline_error:field_undefined(compiler,channel);
	Ch ->
	    case couchbeam_doc:get_value(<<"value">>,JS) of
		undefined ->
		    dripline_error:field_undefined(compiler,value);
		Val ->
		    {ok, I#intermed{channel=Ch,value=Val}}
	    end
    end.

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_get_test() ->
    Test = {[{<<"type">>,<<"command">>},
	     {<<"command">>,{[{<<"do">>,<<"get">>},
			      {<<"channel">>,<<"test">>}]}}]},
    Res = #intermed{type=command,do=get,channel = <<"test">>},
    ?assertEqual({ok, Res},drip_compile(Test)).

basic_set_test() ->
    Test = {[{<<"type">>,<<"command">>},
	     {<<"command">>,{[{<<"do">>,<<"set">>},
			      {<<"channel">>,<<"test">>},
			      {<<"value">>,<<"3500">>}]}}]},
    Res = #intermed{type=command,
		    do=set,
		    channel = <<"test">>, 
		    value = <<"3500">>},
    ?assertEqual({ok,Res},drip_compile(Test)).

-endif.
