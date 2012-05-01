-module(dripline_error).

-export([to_json/1]).
-export([compiler_expected/2,compiler_surprised/2]).
-export([field_undefined/2]).
-export([unsupported_method/2]).

-type dripline_error() :: term().
-export_type([dripline_error/0]).

-spec compiler_expected(term(),term()) -> dripline_error().
compiler_expected(Expected,Received) ->
    {error, [{source,compiler},
	     {expected,Expected},
	     {received,Received}]}.

-spec compiler_surprised(term(),term()) -> dripline_error().
compiler_surprised(Field,Token) ->
    {error, [{source, compiler},
	     {bad_value, Token},
	     {for_field, Field}]}.

-spec field_undefined(term(),term()) -> dripline_error().
field_undefined(Source,Field) ->
    {error, [{source, Source},
	     {field_undefined, Field}]}.

-spec to_json(dripline_error()) -> ejson:ejson_object().
to_json({E,L}) ->
    ejson:encode({[{E, {L}}]}).

-spec unsupported_method(atom(), atom()) -> dripline_error().
unsupported_method(Mod,Method) ->
    {error, [{unsupported_method, [{Mod, Method}]}]}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

compiler_expected_test() ->
    ?assertEqual(to_json(compiler_expected(token1,token2)),
		 <<"{\"error\":{\"source\":\"compiler\",\"expected\":\"token1\",\"received\":\"token2\"}}">>).

compiler_surprised_test() ->
    ?assertEqual(to_json(compiler_surprised(x,y)),
		 <<"{\"error\":{\"source\":\"compiler\",\"bad_value\":\"y\",\"for_field\":\"x\"}}">>).

field_undefined_test() ->
    ?assertEqual(to_json(field_undefined(a,b)),
		 <<"{\"error\":{\"source\":\"a\",\"field_undefined\":\"b\"}}">>).
		 
-endif.
