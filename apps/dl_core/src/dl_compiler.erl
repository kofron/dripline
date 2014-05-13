-module(dl_compiler).
-behaviour(gen_server).

-include_lib("ej/include/ej.hrl").

-record(key_error, {msg, key}).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([compile/1, compiler_error_msg/1]).

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

%%%%%%%%%%%%%%
%%% types  %%%
%%%%%%%%%%%%%%
-type method() :: get | set | run | syscmd.

%%%%%%%%%%%%%%%%%%%%%%%
%%% API definitions %%%
%%%%%%%%%%%%%%%%%%%%%%%
-spec compile(ejson:json_object()) -> 
             {ok, fun()} | {error, dl_error:error()}.
compile(JSON) ->
    gen_server:call(?MODULE,{compile, JSON}).

-spec compiler_error_msg(term()) -> binary().
compiler_error_msg(#ej_invalid{msg=M}) ->
    M;
compiler_error_msg(#key_error{msg=M}) ->
    M.

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
        {error, _E, _Req}=Err ->
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
              {ok, fun()} | {error, dl_error:error()}.
drip_compile(JS) ->
    ID = ej:get({<<"doc">>, <<"_id">>}, JS),
    Command = ej:get({<<"doc">>, <<"command">>}, JS),
    New = dl_request:new(),
    Req = dl_request:set_id(New, ID),
    case json_to_request(Command,Req) of
    {ok, _Request}=Success ->
        Success;
    {error, Reason} ->
        {error, Reason, Req}
    end.

-spec json_to_request(ejson:json_object(), dl_request:dl_request()) ->
                 {ok, dl_request:dl_request()} | {error, term()}.
json_to_request(JS, Req) ->
    case check_request_validity(JS) of
    {ok, Method} -> 
        parse_valid_json(JS, Method, Req);
    {error, _Reason} = E ->
        E
    end. 

-spec check_request_validity(ejson:json_object()) -> 
                    {ok, method()} | {error, term()}.
check_request_validity(JS) ->
    case ej:valid(request_spec(), JS) of
    ok ->
        {ok, verb(JS)};
    BadRequest -> 
        {error, BadRequest}
    end.

-spec verb(ejson:json_object()) -> method().
verb(JS) ->
    Verbs = [
         {get,<<"get">>}, 
         {set,<<"set">>},
         {syscmd,<<"syscmd">>},
         {run, <<"run">>}
        ],
    verb0(JS, Verbs).

-spec verb0(ejson:json_object(), [{method(),binary()}]) -> method().
verb0(JS, [{Verb, Value}|R]) ->
    case ej:get({<<"do">>}, JS) of
    V when V == Value ->
        Verb;
    _AnyOther ->
        verb0(JS, R)
    end.
    
-spec regex_for(atom()) -> {string(), string()}.
regex_for(channel) ->
    {"^[a-z_0-9]+$","channel names consist of a-z & _."};
regex_for(verb) ->
    {"set|get|run|syscmd","valid verbs are set, get, run, and syscmd."};
regex_for(value) ->
    {"[a-zA-Z0-9]+", "value must be a valid string."}.

-spec request_spec() -> ej:ej_json_spec().
request_spec() ->
    {[
      {<<"do">>, {string_match, regex_for(verb)}},
      {<<"channel">>, {string_match, regex_for(channel)}},
      {{opt, <<"value">>}, {string_match, regex_for(value)}}
     ]}.

-spec parse_valid_json(ejson:json_object(), method(), dl_request:dl_request()) -> 
                  dl_request:dl_request().
parse_valid_json(JSON, get, I) ->
    Req = dl_request:set_method(I, get),
    {ok, dl_request:set_target(Req, get_json_channel(JSON))};
parse_valid_json(JSON, set, I) ->
    Req = dl_request:set_method(I, set),
    Req0 = dl_request:set_target(Req, get_json_channel(JSON)),
    case get_json_value(JSON) of
    undefined ->
        Err = key_undef_error(<<"value">>),
        {error, Err};
    Value ->
        {ok, dl_request:set_value(Req0, Value)}
    end;
parse_valid_json(JSON, run, I) -> 
    Req = dl_request:set_method(I, run),
    Req0 = dl_request:set_target(Req, get_json_channel(JSON)),
    % FIXME:
    % Match tuple constructor to de-JSONify args.  This is probably inadequate
    % in the long run.
    {Args} = drop_json_keys([<<"do">>, <<"channel">>], JSON),
    {ok, dl_request:set_value(Req0, Args)}.
                 
-spec get_json_channel(ejson:json_object()) -> binary() | undefined.
get_json_channel(JS) ->
    erlang:binary_to_atom(ej:get({<<"channel">>}, JS), latin1).

-spec get_json_value(ejson:json_object()) -> binary() | undefined.
get_json_value(JS) ->
    ej:get({<<"value">>}, JS).

-spec key_undef_error(binary()) -> term().
key_undef_error(<<"value">>=Key) ->
    #key_error{msg="set commands must have the key \"value\" defined with a legal value.",
           key=Key}.

-spec drop_json_keys([binary()],ejson:json_object()) -> ejson:json_object().
drop_json_keys(KeyList, {JSON}) ->
    drop_json_keys(KeyList, JSON, []).
drop_json_keys(_KeyList, [], Acc) ->
    {Acc};
drop_json_keys(KeyList, [{K,_V}=El|R], Acc) ->
    case lists:member(K, KeyList) of
    true ->
        drop_json_keys(KeyList, R, Acc);
    false ->
        drop_json_keys(KeyList, R, [El|Acc])
    end.

%-spec compile_to_mfa(dl_request:dl_request()) -> {ok, term()}.
%compile_to_mfa(#intermed{type=command, do=run, value=V, channel=Ch}) ->
%%     Args = gen_run_params(V),
%%     {ok, {{unix, ignatius, 0}, read, [Ch, Args]}};
%% compile_to_mfa(#intermed{type=command, do=syscmd, value=V, channel=start_loggers}) ->
%%     Args = lists:map(fun(X) -> erlang:binary_to_atom(X,latin1) end, V),
%%     {ok, {dl_sys, start_loggers, Args}};
%% compile_to_mfa(#intermed{type=command, do=syscmd, value=V, channel=stop_loggers}) ->
%%     Args = lists:map(fun(X) -> erlang:binary_to_atom(X,latin1) end, V),
%%     {ok, {dl_sys, stop_loggers, Args}};
%% compile_to_mfa(#intermed{type=command, do=syscmd, channel=current_loggers}) ->
%%     {ok, {dl_sys, current_loggers, []}};
%% compile_to_mfa(#intermed{type=command, do=get, channel=heartbeat}) ->
%%     {ok, {system, get, heartbeat}};
%% compile_to_mfa(#intermed{type=command, do=get, channel=Ch}) ->
%%     {ok, dl_conf_mgr:get_read_mfa(Ch)};
%% compile_to_mfa(#intermed{type=command, do=set, channel=Ch, value=Val}) ->
%%     {{A, B, C}, D, Args} = dl_conf_mgr:get_write_mfa(Ch),
%%     {ok, {{A,B,C}, D, Args ++ [Val]}}.

%% -spec gen_run_params(ejson:json_object()) -> [{atom(), string()}].
%% gen_run_params(P) ->
%%     lists:map(fun({K,V}) ->
%%               {erlang:binary_to_atom(K,latin1), 
%%                erlang:binary_to_list(V)}
%%           end,
%%           P).

%%%%%%%%%%%%%
%%% EUNIT %%%
%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_compile_good_data() ->
    {[
      {<<"do">>, <<"get">>},
      {<<"channel">>, <<"foo">>}
     ]}.

get_compile_good_result() ->
    R = dl_request:new(),
    R0 = dl_request:set_method(R, get),
    dl_request:set_target(R0, <<"foo">>).

get_compile_good_test() ->
    ?assertEqual(drip_compile(get_compile_good_data()), 
         {ok, get_compile_good_result()}).

set_compile_good_data() ->
    {[
      {<<"do">>, <<"set">>},
      {<<"channel">>, <<"foo">>},
      {<<"value">>, <<"bar">>}
     ]}.

set_compile_good_result() ->
    R = dl_request:new(),
    R0 = dl_request:set_method(R, set),
    R1 = dl_request:set_target(R0, foo),
    dl_request:set_value(<<"bar">>).

set_compile_good_test() ->
    ?assertEqual(drip_compile(set_compile_good_data()),
         {ok, set_compile_good_result()}).


-endif.
