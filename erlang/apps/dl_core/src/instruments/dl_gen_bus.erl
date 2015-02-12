%%%-------------------------------------------------------------------
%%% @author Ben LaRoque
%%% @doc gen_instrument defines a behavior for instrument modules.
%%% It should be the case that all instruments, regardless of bus type,
%%% use this behavior.
%%% @end
%%%-------------------------------------------------------------------
-module(dl_gen_bus).

%%%-------------------------------------------------------------------
%%% Instrument callbacks
%%%-------------------------------------------------------------------
-callback get(Instrument::atom(), Channel::atom()) ->
    Reply::term().

-callback set(atom(), Value::term(), State::term()) ->
    Reply::term().
