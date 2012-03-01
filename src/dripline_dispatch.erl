-module(dripline_dispatch).
-export([dispatch/1]).

dispatch(CouchDBChangeLine) ->
	Doc = proplists:get_value(<<"doc">>,CouchDBChangeLine),
	dispatch_doc(Doc).

dispatch_doc(CmdDoc) ->
	InstrumentID = couchbeam_doc:get_value(<<"instrument">>,CmdDoc),
	case InstrumentID of
		undefined ->
			ok;
		DocId ->
			Conn = dripline_conn_mgr:get(),
			{ok, ConfDb} = couchbeam:open_db(Conn,"dripline_conf"),
		{ok,CmdDb} = couchbeam:open_db(Conn,"dripline_cmd"),
			{ok, InstrDoc} = couchbeam:open_doc(ConfDb,DocId),
			dispatch_instr(CmdDb,CmdDoc,InstrDoc)
	end.

dispatch_instr(Db,CmdDoc, InstrDoc) ->
	Module = couchbeam_doc:get_value(<<"instrument_model">>,InstrDoc),
	CallMod = binary_to_atom(Module),
	InstrId = couchbeam_doc:get_value(<<"instrument_id">>,InstrDoc),
	CallId = binary_to_atom(InstrId),
	{Func,Args} = parse_f_a(CmdDoc),
	Result = CallMod:Func(CallId,Args),
	update_doc(Db, CmdDoc, Result).

update_doc(Db, CmdDoc, CmdResult) ->
	UpDoc = couchbeam_doc:set_value(<<"result">>,CmdResult,CmdDoc),
	NewRev = couchbeam_doc:get_value(<<"_rev">>,CmdDoc),
	UpRes = couchbeam:save_doc(Db,UpDoc),
	io:format("~p~n",[NewRev]).

parse_f_a(CmdDoc) ->
	io:format("~p~n",[CmdDoc]),
	{read,[{1,1}]}.

binary_to_atom(B) ->
	erlang:list_to_atom(erlang:binary_to_list(B)).
