%%%-------------------------------------------------------------------
%%% @author Ben LaRoque
%%% @doc gen_instrument defines a behavior for instrument modules.
%%% It should be the case that all instruments, regardless of bus type,
%%% use this behavior.
%%% @end
%%%-------------------------------------------------------------------
-module(dl_gen_bus).
-behavior(gen_server)

%%%-------------------------------------------------------------------
%%% Instrument callbacks
%%%-------------------------------------------------------------------
-callback init(Args::term()) ->
    {ok, State::term()}.

-callback start_link(CallbackMod::atom(), ID::atom()) ->
    {ok, PID::pid()}|
    ignore|
    {error, Error::term()}.

-callback get(Instrument::atom(), Channel::atom()) ->
    Reply::term().

-callback set(atom(), Value::term(), State::term()) ->
    Reply::term().
