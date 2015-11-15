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

-include("riak_ts_pb.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([encode_rows/2,
         decode_rows/1,
         decode_cells/1,
         encode_field_type/1]).

-type tsrow() :: #tsrow{}.
-export_type([tsrow/0]).

%% types existing between us and eleveldb
-type ldbvalue() :: binary() | number() | boolean() | list().

%% types of #tscell.xxx_value fields, constrained by what protobuf messages accept
%% -type pbvalue() :: binary() | integer() | boolean().
-export_type([ldbvalue/0]).


-spec encode_field_type(atom()) -> atom().
encode_field_type(varchar) ->
    'VARCHAR';
encode_field_type(sint64) ->
    'SINT64';
encode_field_type(double) ->
    'DOUBLE';
encode_field_type(timestamp) ->
    'TIMESTAMP';
encode_field_type(boolean) ->
    'BOOLEAN'.

-spec encode_rows(list(atom()), list(list({binary(), ldbvalue()}))) -> [#tsrow{}].
encode_rows(ColumnTypes, Rows) ->
    [encode_row(ColumnTypes, Row) || Row <- Rows].


-spec decode_rows([#tsrow{}]) -> list(tuple()).
decode_rows(Rows) ->
    [list_to_tuple(decode_cells(Cells)) || #tsrow{cells = Cells} <- Rows].


-spec decode_cells([#tscell{}]) -> list(ldbvalue()).
decode_cells(Cells) ->
    decode_cells(Cells, []).


%% ---------------------------------------
%% local functions

-spec encode_row(list(atom()), list(ldbvalue())) -> #tsrow{}.
encode_row(ColumnTypes, RowCells) ->
    #tsrow{cells = [encode_cell(ColumnTypeCell) || ColumnTypeCell <- lists:zip(ColumnTypes, RowCells)]}.

-spec encode_cell({atom(), ldbvalue()}) -> #tscell{}.
encode_cell({varchar, V}) when is_binary(V) ->
  #tscell{varchar_value = V};
encode_cell({sint64, V}) when is_integer(V) ->
  #tscell{sint64_value = V};
encode_cell({double, V}) when is_float(V) ->
  #tscell{double_value = V};
encode_cell({timestamp, V}) when is_integer(V) ->
  #tscell{timestamp_value = V};
encode_cell({boolean, V}) when is_boolean(V) ->
  #tscell{boolean_value = V};
encode_cell({_ColumnType, undefined}) ->
  #tscell{};
%% NULL Cell
%% TODO: represent null cells by something other than an empty list. emptyTsCell atom maybe?
encode_cell({_ColumnType, []}) ->
  #tscell{}.

-spec decode_cells([#tscell{}], list(ldbvalue())) -> list(ldbvalue()).
decode_cells([], Acc) ->
    lists:reverse(Acc);
decode_cells([#tscell{varchar_value   = Bin,
                      sint64_value    = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      double_value    = undefined} | T], Acc)
  when is_binary(Bin) ->
    decode_cells(T, [Bin | Acc]);
decode_cells([#tscell{varchar_value   = undefined,
                      sint64_value    = Int,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      double_value    = undefined} | T], Acc)
  when is_integer(Int) ->
    decode_cells(T, [Int | Acc]);
decode_cells([#tscell{varchar_value   = undefined,
                      sint64_value    = undefined,
                      timestamp_value = Timestamp,
                      boolean_value   = undefined,
                      double_value    = undefined} | T], Acc)
  when is_integer(Timestamp) ->
    decode_cells(T, [Timestamp | Acc]);
decode_cells([#tscell{varchar_value   = undefined,
                      sint64_value    = undefined,
                      timestamp_value = undefined,
                      boolean_value   = Bool,
                      double_value    = undefined} | T], Acc)
  when is_boolean(Bool) ->
    decode_cells(T, [Bool | Acc]);
decode_cells([#tscell{varchar_value   = undefined,
                      sint64_value    = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      double_value    = Double} | T], Acc)
  when is_float(Double) ->
    decode_cells(T, [Double | Acc]);
decode_cells([#tscell{varchar_value   = undefined,
                      sint64_value    = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      double_value    = undefined} | T], Acc) ->
    %% NULL Cell.
    %% TODO: represent null cells by something other than an empty list. emptyTsCell atom maybe?
    decode_cells(T, [[] | Acc]).
