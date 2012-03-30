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
    case compile_to_rec(JS,New) of
	{ok, Rec} ->
	    case compile_to_fun(Rec) of
		{ok, _F}=Success -> 
		    Success;
		Error -> 
		    Error
	    end;
	Err ->
	    Err
    end.

-spec compile_to_rec(ejson:json_object(),#intermed{}) ->
			    {ok, #intermed{}} | {error, dripline_error:error()}.
compile_to_rec(JS,I) ->
    resolve_type(JS,I).

-spec resolve_type(ejson:ejson_object(), #intermed{}) ->
			  {ok, binary()} | {error, notype}.
resolve_type(JS, Inter) ->
    case props:get(type,JS,undefined) of
	undefined ->
	    dripline_error:compiler_expected(type_field,no_type_field);
	Type ->
	    case lists:member(Type, type_tokens()) of
		true ->
		    AType = dripline_util:binary_to_atom(Type),
		    resolve_action(JS,Inter#intermed{type=AType});
		false ->
		    dripline_error:compiler_surprised(<<"type">>,Type)
	    end
    end.

-spec compile_to_fun(#intermed{}) -> 
			    {ok, fun()} | {error, dripline_error:error()}.
compile_to_fun(#intermed{type=command,do=get,channel=C}) ->
    F =	fun() ->
		case dripline_conf_mgr:lookup(C) of
		    {ok, CD} ->
			(dripline_ch_data:synthesize_fun(CD))();
		    {error, _}=E ->
			fun() -> E end
		end
	end,
    {ok, F};
compile_to_fun(#intermed{type=command,do=set,channel=C,value=V}) ->
    F = fun() ->
		case dripline_conf_mgr:lookup(C) of
		    {ok, CD} ->
			(dripline_ch_data:synthesize_fun(CD,V))();
		    {error, _}=E ->
			fun() -> E end	       
		end
	end,
    {ok, F}.

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
    case props:get(command,JS) of
	undefined ->
	    dripline_error:field_undefined(compiler,command);
	Cmd ->
	    case props:get(do,Cmd) of
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
    case props:get(channel,JS) of
	undefined ->
	    dripline_error:field_undefined(compiler,channel);
	Ch ->
	    {ok, I#intermed{channel=Ch}}
    end;
resolve_target(JS,#intermed{type=command,do=set}=I) ->	
    case props:get(channel,JS) of
	undefined ->
	    dripline_error:field_undefined(compiler,channel);
	Ch ->
	    case props:get(value,JS) of
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
    ?assertEqual(ok,fst(drip_compile(Test))).

basic_set_test() ->
    Test = {[{<<"type">>,<<"command">>},
	     {<<"command">>,{[{<<"do">>,<<"set">>},
			      {<<"channel">>,<<"test">>},
			      {<<"value">>,<<"3500">>}]}}]},
    ?assertEqual(ok,fst(drip_compile(Test))).

fst({A,_B}) ->
    A.

-endif.
