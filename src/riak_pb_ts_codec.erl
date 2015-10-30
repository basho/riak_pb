%% -------------------------------------------------------------------
%%
%% riak_pb_ts_codec.erl: protocol buffer utility functions for Riak TS messages
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

%% @doc Utility functions for decoding and encoding Protocol Buffers
%%      messages related to Riak TS.

-module(riak_pb_ts_codec).

-include("riak_kv_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([encode_columns/1,
         decode_columns/1,
         encode_rows/1,
         encode_cells/1,
         decode_rows/1,
         decode_cells/1,
         encode_field_type/1,
         encode_tsdelreq/3,
         encode_tsgetreq/3]).

-type tsrow() :: #tsrow{}.
-export_type([tsrow/0]).

%% types existing between us and eleveldb
-type ldbvalue() :: binary() | number() | boolean() | list().
%% types of #tscell.xxx_value fields, constrained by what protobuf messages accept
%% -type pbvalue() :: binary() | integer() | boolean().
-export_type([ldbvalue/0]).


-spec encode_field_type(atom()) -> atom().
encode_field_type(binary) ->
    'BINARY';
encode_field_type(integer) ->
    'SINT64';
encode_field_type(float) ->
    'DOUBLE';
encode_field_type(timestamp) ->
    'TIMESTAMP';
encode_field_type(boolean) ->
    'BOOLEAN'.


%% TODO: actually support column specifiers
encode_columns(Columns) ->
    [#tscolumndescription{name = C} || C <- Columns].

decode_columns(Columns) ->
    [C || #tscolumndescription{name = C} <- Columns].


-spec encode_rows(list(list({binary(), ldbvalue()}))) -> [#tsrow{}].
%% @ignore copied from riakc_ts_put_operator; inverse of make_data
encode_rows(Measurements) ->
    [encode_row(M) || M <- Measurements].

-spec encode_cells(list({binary(), ldbvalue()})) -> [#tscell{}].
encode_cells(Cells) ->
    [encode_cell(C) || C <- Cells].


-spec decode_rows([#tsrow{}]) -> list(tuple()).
decode_rows(Rows) ->
    [list_to_tuple(decode_cells(Cells)) || #tsrow{cells = Cells} <- Rows].


-spec decode_cells([#tscell{}]) -> list(ldbvalue()).
decode_cells(Cells) ->
    decode_cells(Cells, []).


encode_tsdelreq(Bucket, Key, Options) ->
    #tsdelreq{table   = Bucket,
              key     = encode_cells(Key),
              vclock  = proplists:get_value(vclock, Options),
              timeout = proplists:get_value(timeout, Options)}.
encode_tsgetreq(Bucket, Key, Options) ->
    #tsgetreq{table   = Bucket,
              key     = encode_cells(Key),
              timeout = proplists:get_value(timeout, Options)}.

%% ---------------------------------------
%% local functions

-spec encode_row(list(ldbvalue())) -> #tsrow{}.
encode_row(Cells) ->
    #tsrow{cells = [encode_cell(C) || C <- Cells]}.

-spec encode_cell(ldbvalue()) -> #tscell{}.
encode_cell(V) when is_binary(V) ->
    #tscell{binary_value = V};
encode_cell(V) when is_integer(V) ->
    #tscell{sint64_value = V};
encode_cell(V) when is_float(V) ->
    #tscell{double_value = V};
encode_cell(V) when is_boolean(V) ->
    #tscell{boolean_value = V};

%% clients can be specific in order to disambiguate between time and
%% plain integer
encode_cell({time, V}) ->
    #tscell{timestamp_value = V};
encode_cell(undefined) ->
    #tscell{};
encode_cell([]) -> %% NULL Cell
    #tscell{}.


-spec decode_cells([#tscell{}], list(ldbvalue())) -> list(ldbvalue()).
decode_cells([], Acc) ->
    lists:reverse(Acc);
decode_cells([#tscell{binary_value    = Bin,
                      sint64_value    = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      double_value    = undefined} | T], Acc)
  when is_binary(Bin) ->
    decode_cells(T, [Bin | Acc]);
decode_cells([#tscell{binary_value    = undefined,
                      sint64_value    = Int,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      double_value    = undefined} | T], Acc)
  when is_integer(Int) ->
    decode_cells(T, [Int | Acc]);
decode_cells([#tscell{binary_value    = undefined,
                      sint64_value    = undefined,
                      timestamp_value = Timestamp,
                      boolean_value   = undefined,
                      double_value    = undefined} | T], Acc)
  when is_integer(Timestamp) ->
    decode_cells(T, [Timestamp | Acc]);
decode_cells([#tscell{binary_value    = undefined,
                      sint64_value    = undefined,
                      timestamp_value = undefined,
                      boolean_value   = Bool,
                      double_value    = undefined} | T], Acc)
  when is_boolean(Bool) ->
    decode_cells(T, [Bool | Acc]);
decode_cells([#tscell{binary_value    = undefined,
                      sint64_value    = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      double_value    = Double} | T], Acc)
  when is_float(Double) ->
    decode_cells(T, [Double | Acc]);
decode_cells([#tscell{binary_value    = undefined,
                      sint64_value    = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      double_value    = undefined} | T], Acc) ->
    %% NULL Cell
    decode_cells(T, [[] | Acc]).
