%% @doc dripline_ch_data is a data structure module that encapsulates
%%		all of the data that dripline has about a particular channel.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dripline_instr_data).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type defs for module %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type id_type() :: binary().
-type module_type() :: atom().
-type support_type() :: atom().
-type bus_type() :: binary().

%%%%%%%%%%%%%%%%%%%
%%% Core record %%%
%%%%%%%%%%%%%%%%%%%
-record(instr_d,{
		id :: id_type(),
		module :: module_type(),
		supports :: [support_type()],
		bus :: bus_type()
	}).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-export([new/0]).
-export([set_id/2,get_id/1]).
-export([set_module/2,get_module/1]).
-export([set_bus/2,get_bus/1]).
-export([set_supports/2,get_supports/1]).
-export([to_childspec/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% types and type exports %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-opaque instr_data() :: #instr_d{}.
-export_type([instr_data/0]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% API Definitions %%% 
%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------%%
%% @doc new/0 returns a new channel data structure.
%% @end
%%---------------------------------------------------------------------%%
-spec new() -> instr_data().
new() ->
	#instr_d{
		id = <<>>,
		module = none,
		supports = [],
		bus = none
	}.

%%---------------------------------------------------------------------%%
%% @doc set_id/3 sets the id field of an instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_id(id_type(),instr_data()) -> instr_data().
set_id(A,D) when is_record(D,instr_d) ->
	D#instr_d{id=A}.

%%---------------------------------------------------------------------%%
%% @doc get_id/3 returns the id field of an instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_id(instr_data()) -> id_type().
get_id(#instr_d{id=Id}) ->
	Id.

%%---------------------------------------------------------------------%%
%% @doc set_module/2 sets the module field of an instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_module(module_type(),instr_data()) -> instr_data().
set_module(A,D) when is_record(D,instr_d) ->
	D#instr_d{module=A}.

%%---------------------------------------------------------------------%%
%% @doc get_module/2 gets the module field of an instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_module(instr_data()) -> module_type().
get_module(#instr_d{module=M}) ->
	M.

%%---------------------------------------------------------------------%%
%% @doc set_supports/2 sets the supports field of an instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_supports(support_type() | [support_type()], instr_data()) ->
		instr_data().
set_supports(A,D) when is_list(A), is_record(D,instr_d) ->
	D#instr_d{supports=A};
set_supports(A,D) when is_record(D,instr_d) ->
	D#instr_d{supports=[A]}.

%%---------------------------------------------------------------------%%
%% @doc get_supports/1 gets the supports field of an instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_supports(instr_data()) -> [support_type()].
get_supports(#instr_d{supports=S}) ->
	S.

%%---------------------------------------------------------------------%%
%% @doc set_bus/2 sets the bus field of an instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec set_bus(bus_type(),instr_data()) -> instr_data().
set_bus(A,D) when is_record(D,instr_d) ->
	D#instr_d{bus=A}.

%%---------------------------------------------------------------------%%
%% @doc get_bus/1 sets the module field of an instr_data object.
%% @end
%%---------------------------------------------------------------------%%
-spec get_bus(instr_data()) -> bus_type().
get_bus(#instr_d{bus=B}) ->
	B.

%%---------------------------------------------------------------------%%
%% @doc to_childspec/1 takes an instr_data record and generates a child
%%		spec that can be used to start a supervised instrument process.
%% @end
%%---------------------------------------------------------------------%%
-spec to_childspec(instr_data()) -> supervisor:child_spec().
to_childspec(#instr_d{bus=B,module=Mod,id=Name}) ->
    {_BusType,Bus,Addr} = parse_bus(B),
    N = dripline_util:binary_to_atom(Name),
    M = dripline_util:binary_to_atom(Mod),
    {
      N, % registered name of the instrument
      {
	M, % module
	start_link,
	[N,Bus,Addr] % need to have parsed bus info here
      },
      permanent,
      5000,
      worker,
      [M]
    }.

-spec parse_bus(binary()) -> {atom(),atom(),integer()}.
parse_bus(BusInfo) ->
    StrInfo = erlang:binary_to_list(BusInfo),
    [ModStr,BusIDStr,AddrStr] = string:tokens(StrInfo,"/:"),
    [Mod,BusID] = lists:map(fun(X) -> 
				    erlang:list_to_atom(X) 
			    end,
			    [ModStr,BusIDStr]),
    {Addr,[]} = string:to_integer(AddrStr),
    {Mod,BusID,Addr}.
