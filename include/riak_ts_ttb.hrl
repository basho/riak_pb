-define(TTB_MSG_CODE, 104).

-ifndef(TSTTBPUTREQ_H).
-define(TSTTBPUTREQ_H, true).
-record(tsttbputreq, {
    table = erlang:error({required, table}),
    columns = [],
    rows = []
}).
-endif.

-ifndef(TSTTBPUTRESP_H).
-define(TSTTBPUTRESP_H, true).
-record(tsttbputresp, {
    
}).
-endif.

-ifndef(TSTTBQUERYREQ_H).
-define(TSTTBQUERYREQ_H, true).
-record(tsttbqueryreq, {
    query,
    stream = false,
    cover_context
}).
-endif.

-ifndef(TSTTBQUERYRESP_H).
-define(TSTTBQUERYRESP_H, true).
-record(tsttbqueryresp, {
    columns = [],
    rows = [],
    done = true
}).
-endif.
