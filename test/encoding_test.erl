-module(encoding_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("riak_pb_kv_codec.hrl").
-include("riak_pb.hrl").

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
                               {?MD_USERMETA, [{<<"X-Riak-Meta-MyMetaData1">>, <<"here it is">>},
                                               {<<"X-Riak-Meta-MoreMd">>, <<"have some more">>},
                                               {<<"X-Riak-Meta-EvenMoreMd">>, term_to_binary({a,b,c,d,e,f})}
                                              ]},
                               {?MD_INDEX, [{<<"index_bin">>, <<"foo">>}]},
                               {?MD_DELETED, true}
                              ]),
                 Value = <<"test value">>,
                 {MetaData2, Value2} = riak_pb_kv_codec:decode_content(
                                         riak_kv_pb:decode_rpbcontent(
                                           riak_kv_pb:encode_rpbcontent(
                                             riak_pb_kv_codec:encode_content({MetaData, Value})))),
                 ?assertEqual(lists:sort(dict:to_list(MetaData)), lists:sort(dict:to_list(MetaData2))),
                 ?assertEqual(Value, Value2)
             end)},
     {"deleted header encode decode",
      ?_test(begin
                 InputMD = [dict:from_list([{?MD_DELETED, DelVal}]) ||
                               DelVal <- [true, "true", false, <<"rubbish">>]],
                 Value = <<"test value">>,
                 {OutputMD, _} = lists:unzip(
                                   [riak_pb_kv_codec:decode_content(
                                      riak_kv_pb:decode_rpbcontent(
                                        riak_kv_pb:encode_rpbcontent(
                                          riak_pb_kv_codec:encode_content({MD, Value})))) ||
                                       MD <- InputMD]),
                 MdSame1 = (lists:sort(dict:to_list(lists:nth(1, OutputMD))) =:=
                                lists:sort(dict:to_list(lists:nth(2, OutputMD)))),
                 MdSame2 = (lists:sort(dict:to_list(lists:nth(3, OutputMD))) =:=
                                lists:sort(dict:to_list(lists:nth(4, OutputMD)))),
                 ?assertEqual(true, MdSame1),
                 ?assertEqual(true, MdSame2)
             end)},
     {"empty content encode decode",
      ?_test(begin
                 MetaData = dict:new(),
                 Value = <<"test value">>,
                 {MetaData2, Value2} = riak_pb_kv_codec:decode_content(
                                         riak_kv_pb:decode_rpbcontent(
                                           riak_kv_pb:encode_rpbcontent(
                                             riak_pb_kv_codec:encode_content({MetaData, Value})))),
                 ?assertEqual([], dict:to_list(MetaData2)),
                 ?assertEqual(Value, Value2)
             end)},
     {"empty repeated metas are removed/ignored",
      ?_test(begin
                 MetaData = dict:from_list([{?MD_LINKS, []}, {?MD_USERMETA, []}, {?MD_INDEX, []}]),
                 Value = <<"test value">>,
                 {MetaData2, Value2} = riak_pb_kv_codec:decode_content(
                                         riak_kv_pb:decode_rpbcontent(
                                           riak_kv_pb:encode_rpbcontent(
                                             riak_pb_kv_codec:encode_content({MetaData, Value})))),
                 ?assertEqual([], dict:to_list(MetaData2)),
                 ?assertEqual(Value, Value2)
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
             end)},
     {"Int index rpbpair",
      ?_test(begin
                 Index = #rpbpair{key="index_int", value=100},
                 ErlangyFy = riak_pb_codec:decode_pair(Index),
                 PBify = riak_pb_codec:encode_pair(ErlangyFy),
                 ?assertEqual({rpbpair, <<"index_int">>, <<"100">>}, PBify)
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
