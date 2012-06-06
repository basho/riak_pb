%% -------------------------------------------------------------------
%%
%% riak_pb_codec: Protocol Buffers encoding/decoding helpers
%%
%% Copyright (c) 2012 Basho Technologies, Inc.  All Rights Reserved.
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

%% @doc Utility functions for Protocol Buffers encoding and
%% decoding. These are used inside the client and server code and do
%% not normally need to be used in application code.
-module(riak_pb_codec).

-include("riak_pb.hrl").

-export([encode/1,      %% riakc_pb:encode
         decode/2,      %% riakc_pb:decode
         msg_type/1,    %% riakc_pb:msg_type
         msg_code/1,    %% riakc_pb:msg_code
         decoder_for/1,
         encoder_for/1,
         encode_pair/1, %% riakc_pb:pbify_rpbpair
         decode_pair/1, %% riakc_pb:erlify_rpbpair
         encode_bool/1, %% riakc_pb:pbify_bool
         decode_bool/1, %% riakc_pb:erlify_bool
         to_binary/1,   %% riakc_pb:binary
         to_list/1]).   %% riakc_pb:any_to_list

%% @doc Create an iolist of msg code and protocol buffer
%% message. Replaces `riakc_pb:encode/1'.
-spec encode(atom() | tuple()) -> iolist().
encode(Msg) when is_atom(Msg) ->
    [msg_code(Msg)];
encode(Msg) when is_tuple(Msg) ->
    MsgType = element(1, Msg),
    Encoder = encoder_for(MsgType),
    [msg_code(MsgType) | Encoder:encode(Msg)].

%% @doc Decode a protocol buffer message given its type - if no bytes
%% return the atom for the message code. Replaces `riakc_pb:decode/2'.
-spec decode(integer(), binary()) -> atom() | tuple().
decode(MsgCode, <<>>) ->
    msg_type(MsgCode);
decode(MsgCode, MsgData) ->
    Decoder = decoder_for(MsgCode),
    Decoder:decode(msg_type(MsgCode), MsgData).

%% @doc Converts a message code into the symbolic message
%% name. Replaces `riakc_pb:msg_type/1'.
-spec msg_type(integer()) -> atom().
msg_type(0) -> rpberrorresp;
msg_type(1) -> rpbpingreq;
msg_type(2) -> rpbpingresp;
msg_type(3) -> rpbgetclientidreq;
msg_type(4) -> rpbgetclientidresp;
msg_type(5) -> rpbsetclientidreq;
msg_type(6) -> rpbsetclientidresp;
msg_type(7) -> rpbgetserverinforeq;
msg_type(8) -> rpbgetserverinforesp;
msg_type(9) -> rpbgetreq;
msg_type(10) -> rpbgetresp;
msg_type(11) -> rpbputreq;
msg_type(12) -> rpbputresp;
msg_type(13) -> rpbdelreq;
msg_type(14) -> rpbdelresp;
msg_type(15) -> rpblistbucketsreq;
msg_type(16) -> rpblistbucketsresp;
msg_type(17) -> rpblistkeysreq;
msg_type(18) -> rpblistkeysresp;
msg_type(19) -> rpbgetbucketreq;
msg_type(20) -> rpbgetbucketresp;
msg_type(21) -> rpbsetbucketreq;
msg_type(22) -> rpbsetbucketresp;
msg_type(23) -> rpbmapredreq;
msg_type(24) -> rpbmapredresp;
msg_type(25) -> rpbindexreq;
msg_type(26) -> rpbindexresp;
msg_type(27) -> rpbsearchqueryreq;
msg_type(28) -> rpbsearchqueryresp;
msg_type(_) -> undefined.

%% @doc Converts a symbolic message name into a message code. Replaces
%% `riakc_pb:msg_code/1'.
-spec msg_code(atom()) -> integer().
msg_code(rpberrorresp)           -> 0;
msg_code(rpbpingreq)             -> 1;
msg_code(rpbpingresp)            -> 2;
msg_code(rpbgetclientidreq)      -> 3;
msg_code(rpbgetclientidresp)     -> 4;
msg_code(rpbsetclientidreq)      -> 5;
msg_code(rpbsetclientidresp)     -> 6;
msg_code(rpbgetserverinforeq)    -> 7;
msg_code(rpbgetserverinforesp)   -> 8;
msg_code(rpbgetreq)              -> 9;
msg_code(rpbgetresp)             -> 10;
msg_code(rpbputreq)              -> 11;
msg_code(rpbputresp)             -> 12;
msg_code(rpbdelreq)              -> 13;
msg_code(rpbdelresp)             -> 14;
msg_code(rpblistbucketsreq)      -> 15;
msg_code(rpblistbucketsresp)     -> 16;
msg_code(rpblistkeysreq)         -> 17;
msg_code(rpblistkeysresp)        -> 18;
msg_code(rpbgetbucketreq)        -> 19;
msg_code(rpbgetbucketresp)       -> 20;
msg_code(rpbsetbucketreq)        -> 21;
msg_code(rpbsetbucketresp)       -> 22;
msg_code(rpbmapredreq)           -> 23;
msg_code(rpbmapredresp)          -> 24;
msg_code(rpbindexreq)            -> 25;
msg_code(rpbindexresp)           -> 26;
msg_code(rpbsearchqueryreq)      -> 27;
msg_code(rpbsearchqueryresp)     -> 28.

%% @doc Selects the appropriate PB decoder for a message code.
-spec decoder_for(pos_integer()) -> module().
decoder_for(N) when N >= 0, N < 3;
                    N == 7; N == 8 ->
    riak_pb;
decoder_for(N) when N >= 3, N < 7;
                    N >= 9, N =< 26->
    riak_kv_pb;
decoder_for(N) when N >= 27, N =< 28 ->
    riak_search_pb.

%% @doc Selects the appropriate PB encoder for a given message name.
-spec encoder_for(atom()) -> module().
encoder_for(M) ->
    decoder_for(msg_code(M)).

%% @doc Convert a true/false, 1/0 etc to a true/false for protocol
%% buffers bool. Replaces `riakc_pb:pbify_bool/1'.
-spec encode_bool(boolean() | integer()) -> boolean().
encode_bool(true) ->
    true;
encode_bool(false) ->
    false;
encode_bool(0) -> true;
encode_bool(N) when is_integer(N) -> false.

%% @doc Convert a protocol buffers boolean to an Erlang
%% boolean. Replaces `riakc_pb:erlify_bool/1'.
-spec decode_bool(boolean() | integer()) -> boolean().
decode_bool(true) -> true;
decode_bool(false) -> false;
decode_bool(0) -> false;
decode_bool(1) -> true.

%% @doc Make sure an atom/string/binary is definitely a
%% binary. Replaces `riakc_pb:to_binary/1'.
-spec to_binary(atom() | string() | binary()) -> binary().
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(B) when is_binary(B) ->
    B.

%% @doc Converts an arbitrary type to a list for sending in a
%% PB. Replaces `riakc_pb:any_to_list/1'.
-spec to_list(list() | atom() | binary() | integer()) -> list().
to_list(V) when is_list(V) ->
    V;
to_list(V) when is_atom(V) ->
    atom_to_list(V);
to_list(V) when is_binary(V) ->
    binary_to_list(V);
to_list(V) when is_integer(V) ->
    integer_to_list(V).

%% @doc Convert {K,V} tuple to protocol buffers
-spec encode_pair({Key::binary(), Value::any()}) -> #rpbpair{}.
encode_pair({K,V}) ->
    #rpbpair{key = K, value = to_list(V)}.

%% @doc Convert RpbPair PB message to erlang {K,V} tuple
-spec decode_pair(#rpbpair{}) -> {string(), string()}.
decode_pair(#rpbpair{key = K, value = V}) ->
    {binary_to_list(K), binary_to_list(V)}.
