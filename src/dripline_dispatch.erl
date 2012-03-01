-module(dripline_dispatch).
-export([dispatch/1]).

dispatch(CouchDBChangeLine) ->
	Doc = proplists:get_value(<<"doc">>,CouchDBChangeLine),
	dispatch_doc(Doc).

dispatch_doc(CmdDoc) ->
	Type = couchbeam_doc:get_value(<<"type">>,CmdDoc),
	case Type of
		<<"query">> ->
			dispatch_query(CmdDoc);
		_ ->
			{error,{unknown_cmd_type,Type}}
	end.

dispatch_query(CmdDoc) ->
	{QString} = couchbeam_doc:get_value(<<"query">>,CmdDoc),
	QStringE = fetch_channel_info(QString),
	D = proplists:get_value(<<"read">>,QStringE),
	Call = dict:fetch(read,D),
	Args = dict:fetch(locator,D),
	Result = Call(Args),
	ServConn = dripline_conn_mgr:get(),
	{ok, Db} = couchbeam:open_db(ServConn,"dripline_cmd"),
	update_doc(Db,Result,CmdDoc).

update_doc(Db, CmdResult, CmdDoc) ->
	UpDoc = couchbeam_doc:set_value(<<"result">>,CmdResult,CmdDoc),
	NewRev = couchbeam_doc:get_value(<<"_rev">>,CmdDoc),
	Id = couchbeam_doc:get_value(<<"_id">>,CmdDoc),
	NewRevNo = strip_rev_no(NewRev) + 1,
	spawn(fun() -> dripline_cmd_mon:notify(Id,NewRevNo) end),
	couchbeam:save_doc(Db,UpDoc).

strip_rev_no(BinRev) ->
	[NS,_] = string:tokens(binary_to_list(BinRev),"-"),
	{N,[]} = string:to_integer(NS),
	N.

fetch_channel_info(QueryString) ->
	QS1 = lists:keymap(fun(X) -> 
			lists:map(fun(Y) -> 
				dripline_conf_mgr:lookup(channel,Y) end,X)
	end, 2, QueryString),
	merge_commands(QS1).

merge_commands(CommandList) ->
	merge_commands(CommandList,[]).
merge_commands([],Acc) ->
	Acc;
merge_commands([{Cmd,DictList}|T],Acc) ->
	MergedDicts = merge_dicts(DictList),
	merge_commands(T,[{Cmd,MergedDicts}|Acc]).

merge_dicts([]) ->
	dict:new();
merge_dicts(List) ->
	merge_dicts(List,dict:new()).
merge_dicts([],Acc) ->
	Acc;
merge_dicts([D|T],Acc) ->
	MD = dict:merge(fun(K,V1,V2) -> merge_dict_keys(K,V1,V2) end,D,Acc),
	merge_dicts(T,MD).

merge_dict_keys(_K,V1,V1) ->
	V1;
merge_dict_keys(_K,V1,V2) when is_list(V1) ->
	[V2|V1];
merge_dict_keys(_K,V1,V2) when is_list(V2) ->
	[V1|V2];
merge_dict_keys(_K,V1,V2) ->
	[V1,V2].