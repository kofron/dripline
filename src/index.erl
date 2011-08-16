-module (index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("basedir.hrl").

main() ->
     #template { file="templates/base.html" }.

title() ->
    "my little counter".

body() ->
    wf:comet(fun() ->
		     counter(3) 
	     end),
    #panel { id=placeholder }.

counter(Count) ->
        timer:sleep(1000),
        wf:update(placeholder, integer_to_list(Count)),
        wf:flush(),
        counter(Count + 2).
