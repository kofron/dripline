%% @doc dl_request defines an opaque data type, the dl_request, which encapsulates information
%% about external requests.
-module(dl_request).
-export([new/0,
	 set_id/2,
	 get_id/1,
	 set_method/2,
	 get_method/1,
	 set_target/2,
	 get_target/1,
	 set_value/2,
	 get_value/1
	]).

-record(dl_request, {
	  id,
	  method,
	  target,
	  value
}).

-type method() :: get | set | run | syscmd.
-type id() :: binary().

-opaque dl_request() :: #dl_request{}.
-export_type([dl_request/0, id/0, method/0]).

-spec new() -> dl_request().
new() ->
    #dl_request{}.

-spec set_id(dl_request(), id()) -> dl_request().
set_id(DLR, ID) ->
    #dl_request{id=ID}.

-spec get_id(dl_request()) -> id().
get_id(#dl_request{id=ID}) ->
    ID.

-spec set_method(dl_request(), method()) -> dl_request().
set_method(DLR, M) ->
    DLR#dl_request{method = M}.

-spec get_method(dl_request()) -> method().
get_method(#dl_request{method=M}) ->
    M.

-spec set_target(dl_request(), atom()) -> dl_request().
set_target(DLR, T) ->
    DLR#dl_request{target = T}.

-spec get_target(dl_request()) -> atom().
get_target(#dl_request{target=T}) ->
    T.

-spec set_value(dl_request(), term()) -> dl_request().
set_value(DLR, V) ->
    DLR#dl_request{value = V}.

-spec get_value(dl_request()) -> term().
get_value(#dl_request{value = V}) ->
    V.

