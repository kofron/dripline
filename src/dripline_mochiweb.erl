-module(dripline_mochiweb).
-export ([loop/1]).
-include_lib("nitrogen_core/include/wf.hrl").

loop(Req) ->    
    DocRoot = "./static",
    RequestBridge = simple_bridge:make_request(mochiweb_request_bridge, {Req, DocRoot}),
    ResponseBridge = simple_bridge:make_response(mochiweb_response_bridge, {Req, DocRoot}),
    nitrogen:init_request(RequestBridge, ResponseBridge),
    nitrogen:run().
