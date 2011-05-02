% dripline.erl
% written by jared kofron <jared.kofron@gmail.com>
% interface module to the dripline system.  abstracts away
% procedures such as reading data, writing configurations, and
% so on and so forth.
-module(dripline).

% main API functions
-export([read/1]).

read(Card) -> 
    case legal_card(Card) of
	true ->
	    ;
	false ->
	    erlang:error("Card invalid!")
    end.
    
% 
-spec legal_card(term(())) -> boolean().
legal_card(cardA) ->
    true;
legal_card(cardB) ->
    true;
legal_card(cardC) ->
    true;
legal_card(cardD) ->
    true;
legal_card(_) ->
    false.




