%% @doc dripline_util contains functions that are used all over
%%		the code base as helpers.
%% @author jared kofron <jared.kofron@gmail.com>
-module(dripline_util).
-export([binary_to_atom/1]).

%%---------------------------------------------------------------------%%
%% @doc binary_to_atom simply converts a binary string into an atom.
%% @end
%%---------------------------------------------------------------------%%
-spec binary_to_atom(binary()) -> atom().
binary_to_atom(Binary) ->
	erlang:list_to_atom(erlang:binary_to_list(Binary)).