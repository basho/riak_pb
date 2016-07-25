%% -------------------------------------------------------------------
%%
%% test cases for riak_pb_dt_codec: Protocol Buffers utility functions for Riak DT types
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(riak_pb_dt_codec_tests).

-include_lib("eunit/include/eunit.hrl").

-include("riak_dt_pb.hrl").

-import(riak_pb_dt_codec, [decode_operation/1, operation_type/1, decode_fetch_response/1, encode_fetch_response/4]).

-define(CONTEXT,undefined_context).
-define(SET_VALUE,  [<<"binarytemple">>] ).

decode_operation_gset_test() ->
  Op = #dtop{set_op = #gsetop{adds = ?SET_VALUE }},
  OpDecode = decode_operation(Op),
  ?assertEqual(OpDecode, {add_all, ?SET_VALUE }).

operation_type_gset_test() ->
  OpType = operation_type(#dtop{set_op = #gsetop{}}),
  ?assertEqual(OpType, gset).

decode_fetch_response_gset_test() ->
  Res = decode_fetch_response(#dtfetchresp{context =  ?CONTEXT, type = 'GSET', value = #dtvalue{set_value = ?SET_VALUE }}),
  ?assertEqual(Res , {gset,?SET_VALUE ,?CONTEXT} ).

encode_fetch_response_gset_test() ->
 Resp =   encode_fetch_response(gset, #dtvalue{set_value = ?SET_VALUE } , ?CONTEXT, []),
  ?debugVal(Resp),
  ?assertEqual(Resp,
    {dtfetchresp,?CONTEXT,'GSET',
      {dtvalue,undefined,
        {dtvalue,undefined,?SET_VALUE,[]},
        []}}
    )
 .