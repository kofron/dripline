% gets ERL_ROOT
get_stripped_root() ->
    KernDir = code:priv_dir(kernel),
    Stripped = lists:nthtail(3,lists:reverse(re:split(KernDir,"/"))),    
    lists:nthtail(1,lists:reverse(Stripped)).

stringify(Arg) ->
    stringify(Arg,[]).
stringify([],Acc) ->
    Acc;
stringify([H|T],Acc) when is_binary(H)->
    stringify(T,Acc ++ [<<$/>>] ++ [H]).

binary_cat(A,B) when is_binary(A), is_binary(B) ->
    <<A/binary,B/binary>>.

main([]) ->
    R = lists:foldr(fun(A,B) -> binary_cat(A,B) end, 
		    <<>>,
		    stringify(get_stripped_root())),
    io:format("~s",[binary_to_list(R)]).
