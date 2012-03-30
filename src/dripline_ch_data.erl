%% @doc dripline_ch_data is a data structure module that encapsulates
%%		all of the data that dripline has about a particular channel.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dripline_ch_data).

%%%%%%%%%%%%%%%%%%%
%%% Core record %%%
%%%%%%%%%%%%%%%%%%%
-record(cd,{
		id :: binary(),
		instr :: binary(),
		model :: atom(),
		locator :: term()
	}).

-opaque ch_data() :: #cd{}.
-export_type([ch_data/0]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([new/0,from_json/1]).
-export([get_fields/2,get_fields/3,set_field/3]).
-export([synthesize_fun/1,synthesize_fun/2]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API Definitions %%% 
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc new/0 returns a new channel data structure.
%% @end
%%---------------------------------------------------------------------%%
-spec new() -> record().
new() ->
	#cd{
		id = <<>>,
		instr = none,
		model = none,
		locator = none
	}.
%%---------------------------------------------------------------------%%
%% @doc from_json/1 returns a new channel data structure that is built 
%%		from a json object with the appropriate fields.
%% @end
%%---------------------------------------------------------------------%%
-spec from_json(ejson:json_object()) -> 
	{ok, ch_data()} | {error, term()}.
from_json(JS) ->
	N = new(),
	make_ch_data(N,JS).

-spec make_ch_data(ch_data(),ejson:json_object()) ->
	{ok, ch_data()} | {error, term()}.
make_ch_data(N, JS) ->
	set_id(N,JS).
set_id(N,JS) ->
	case props:get(name,JS) of
		undefined ->
			{error, {required, name}};
		Val ->
			set_loc(N#cd{id=Val},JS)
	end.
set_loc(N,JS) ->
	case props:get(locator,JS) of
		undefined ->
			{error, {required, locator}};
		Val ->
			set_instr(N#cd{locator=Val},JS)
	end.
set_instr(N,JS) ->
	case props:get(instrument,JS) of
		undefined ->
			{error, {required, instrument}};
		Val ->
			set_model(N#cd{instr=Val},JS)
	end.
set_model(#cd{instr=I}=N,_JS) ->
	case dripline_conf_mgr:lookup_instr(I) of
		{ok, InD} ->
			Mod = dripline_instr_data:get_module(InD),
			{ok, set_field(model,Mod,N)};
		_ ->
			{error, {required, model}}
	end.
%%---------------------------------------------------------------------%%
%% @doc set_field/3 sets the value of a field in an existing record to a
%%		new value.
%% @end
%%---------------------------------------------------------------------%%
-spec set_field(atom(),term(),record()) -> record().
set_field(id, V, R) when is_record(R,cd) ->
	R#cd{id=V};
set_field(instr, V, R) when is_record(R,cd) ->
	R#cd{instr=V};
set_field(model, V, R) when is_record(R,cd) ->
	R#cd{model=V};
set_field(locator, V, R) when is_record(R,cd) ->
	R#cd{locator=V};
set_field(Any, _V, R) when is_record(R,cd) ->
	{error,{bad_field, Any}}.

%%---------------------------------------------------------------------%%
%% @doc get_fields/2 returns a field or list of fields from the data
%%		structure.
%% @end
%%---------------------------------------------------------------------%%
-spec get_fields(atom() | [atom()], record()) -> 
		{ok,term()} | {ok,[term()]} | {error, term()}.
get_fields(id,#cd{id=Id}) ->
	{ok,Id};
get_fields(model,#cd{model=Md}) ->
	{ok,Md};
get_fields(instr,#cd{instr=In}) ->
	{ok,In};
get_fields(locator,#cd{locator=Lc}) ->
	{ok,Lc};
get_fields(Any,_Rec) when is_atom(Any) ->
	{error,{bad_field, Any}};
get_fields(Fields,Rec) when is_list(Fields) ->
	get_fields(Fields,Rec,[]).
get_fields([],_Rec,Acc) ->
	{ok,lists:reverse(Acc)};
get_fields([H|T],Rec,Acc) ->
	case get_fields(H,Rec) of
		{error,{bad_field,_}}=E ->
			E;
		{ok,Data} ->
			get_fields(T,Rec,[Data|Acc])
	end.

%%---------------------------------------------------------------------%%
%% @doc synthesize_fun/1 takes a channel data structure and generates a
%%		fun of arity 0 that will read the channel in question.
%% @end
%%---------------------------------------------------------------------%%
-spec synthesize_fun(record()) -> fun(() -> binary()).
synthesize_fun(#cd{instr=BI,model=BM,locator=L}) ->
	[I,M] = lists:map(fun(X) -> 
						dripline_util:binary_to_atom(X) 
					end, 
					[BI,BM]),
	fun() -> M:read(I,L) end.

%%---------------------------------------------------------------------%%
%% @doc synthesize_fun/2 takes a channel data structure and a value to 
%%		write to the channel and generates a fun of arity 0 that will
%%		perform the writing.
%% @end
%%---------------------------------------------------------------------%%
-spec synthesize_fun(record(),binary()) -> fun(() -> binary()).
synthesize_fun(#cd{instr=BI,model=BM,locator=L},V) ->
	[I,M] = lists:map(fun(X) -> 
						dripline_util:binary_to_atom(X) 
					end, 
					[BI,BM]),
	fun() -> M:write(I,L,V) end.
