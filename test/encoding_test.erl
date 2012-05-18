-module(encoding_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("riak_pb_kv_codec.hrl").

pb_test_() ->
    [{"content encode decode",
      ?_test(begin
                 MetaData = dict:from_list(
                              [{?MD_CTYPE, "ctype"},
                               {?MD_CHARSET, "charset"},
                               {?MD_ENCODING, "encoding"},
                               {?MD_VTAG, "vtag"},
                               {?MD_LINKS, [{{<<"b1">>, <<"k1">>}, <<"v1">>},
                                            {{<<"b2">>, <<"k2">>}, <<"v2">>}
                                           ]},
                               {?MD_LASTMOD, {1, 2, 3}},
                               {?MD_USERMETA, [{"X-Riak-Meta-MyMetaData1","here it is"},
                                               {"X-Riak-Meta-MoreMd", "have some more"}
                                              ]}
                              ]),
                 Value = <<"test value">>,
                 {MetaData2, Value2} = riak_pb_kv_codec:decode_content(
                                         riak_kv_pb:decode_rpbcontent(
                                           riak_kv_pb:encode_rpbcontent(
                                             riak_pb_kv_codec:encode_content({MetaData, Value})))),
                 MdSame = (lists:sort(dict:to_list(MetaData)) =:=
                               lists:sort(dict:to_list(MetaData2))),
                 ?assertEqual(true, MdSame),
                 Value = Value2
             end)},
     {"empty content encode decode",
      ?_test(begin
                 MetaData = dict:new(),
                 Value = <<"test value">>,
                 {MetaData2, Value2} = riak_pb_kv_codec:decode_content(
                                         riak_kv_pb:decode_rpbcontent(
                                           riak_kv_pb:encode_rpbcontent(
                                             riak_pb_kv_codec:encode_content({MetaData, Value})))),
                 MdSame = (lists:sort(dict:to_list(MetaData)) =:=
                               lists:sort(dict:to_list(MetaData2))),
                 ?assertEqual(true, MdSame),
                 Value = Value2
             end)},
     {"msg code encode decode",
      ?_test(begin
                 msg_code_encode_decode(0)
             end)},
     {"bucket props encode decode",
      ?_test(begin
                 Props = [{n_val, 99},
                          {allow_mult, true}],
                 Props2 = riak_pb_kv_codec:decode_bucket_props(
                            riak_kv_pb:decode_rpbbucketprops(
                              riak_kv_pb:encode_rpbbucketprops(
                                riak_pb_kv_codec:encode_bucket_props(Props)))),
                 MdSame = (lists:sort(Props) =:=
                               lists:sort(Props2)),
                 ?assertEqual(true, MdSame)
             end)},
     {"bucket props encode decode 2",
      ?_test(begin
                 Props = [{n_val, 33},
                          {allow_mult, false}],
                 Props2 = riak_pb_kv_codec:decode_bucket_props(
                            riak_kv_pb:decode_rpbbucketprops(
                              riak_kv_pb:encode_rpbbucketprops(
                                riak_pb_kv_codec:encode_bucket_props(Props)))),
                 MdSame = (lists:sort(Props) =:=
                               lists:sort(Props2)),
                 ?assertEqual(true, MdSame)
             end)}
    ]
        .


msg_code_encode_decode(N) ->
    case riak_pb_codec:msg_type(N) of
        undefined ->
            ok;
        MsgType ->
            ?assertEqual(N, riak_pb_codec:msg_code(MsgType)),
            msg_code_encode_decode(N+1)
    end.
