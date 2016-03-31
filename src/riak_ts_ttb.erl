%% -------------------------------------------------------------------
%%
%% riak_ts_ttb.erl: term-to-binary utility functions for Riak TS messages
%%
%% Copyright (c) 2015 Basho Technologies, Inc.  All Rights Reserved.
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

%% @doc Utility functions for decoding and encoding term-to-binary
%%      messages related to Riak TS.

-module(riak_ts_ttb).

-export([encode/1,
	 decode/2]).

%% ------------------------------------------------------------
%% Encode for TTS simply converts any strings to binary and encodes to
%% erlang binary format
%% ------------------------------------------------------------

encode(Msg) ->
    T2B=term_to_binary(de_stringify(Msg)),
    <<T2B/binary>>.

%% ------------------------------------------------------------
%% Decode does the reverse
%% ------------------------------------------------------------

decode(_MsgType, MsgData) ->
    return_resp(binary_to_term(MsgData)).

%% ------------------------------------------------------------
%% But if the decoded response is empty, just return the atom
%% identifying the message.  This mimics the behavior of the PB
%% decoder, which simply returns msg_type(msg_code) if the message
%% body is empty
%% ------------------------------------------------------------

return_resp({Atom, <<>>}) ->
    Atom;
return_resp(Resp) ->
    Resp.

de_stringify(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(de_stringify(tuple_to_list(Tuple)));
de_stringify(List) when is_list(List), is_integer(hd(List)) ->
    %% Yes, this could corrupt utf-8 data, but we should never, ever
    %% have put it in string format to begin with
    list_to_binary(List);
de_stringify(List) when is_list(List) ->
    lists:map(fun de_stringify/1, List);
de_stringify(Element) ->
    Element.

