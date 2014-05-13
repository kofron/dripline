%%%-------------------------------------------------------------------
%%% @author Ben LaRoque
%%% @doc gen_instrument defines a behavior for instrument modules.
%%% It should be the case that all instruments, regardless of bus type,
%%% use this behavior.
%%% @end
%%%-------------------------------------------------------------------
-module(gen_instrument).

%%%-------------------------------------------------------------------
%%% Instrument callbacks
%%%-------------------------------------------------------------------
-callback init(Args::term()) ->
    {ok, State::term()}.

-callback start_link(ID::atom()) ->
    {ok, PID::pid()}|
    ignore|
    {error, Error::term()}.

-callback handle_get(atom(), State::term()) ->
    {send, term(), State::term()}|
    {error, {Type::atom(), {Reason::atom(), Detail::term()}}, NewState::term()}.

-callback handle_set(atom(), Value::term(), State::term()) ->
    {send, term(), State::term()}|
    {error, {Type::atom(), {Reason::atom(), Detail::term()}}, NewState::term()}.
