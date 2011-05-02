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
	    route_read_request(Card);
	false ->
	    invalid_card_req(Card,read)
    end.
    
-spec route_read_request(atom()) -> term().
route_read_request(Card) ->
    gen_fsm:sync_send_event(Card,read).

invalid_card_req(Card,Op) when is_atom(Op), is_atom(Card) ->
    erlang:error("invalid operation " ++
		     atom_to_list(Op) ++
		     " for card " ++
		     atom_to_list(Card)).

% 
-spec legal_card(term()) -> boolean().
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
