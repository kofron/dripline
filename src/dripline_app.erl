%% @doc dripline_app is the top level OTP application module for 
%%		dripline.
%% @author jared kofron <jared.kofron@gmail.com>
%% @version 0.1a

-module(dripline_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    dripline_sup:start_link().

stop(_State) ->
    ok.
