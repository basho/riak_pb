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
         decode_rows/1,
         encode_field_type/1]).

-define(SINT64_MIN, -16#8000000000000000).
-define(SINT64_MAX,  16#7FFFFFFFFFFFFFFF).

%% types existing between us and eleveldb
-type ldbvalue() :: binary() | number() | boolean() | list().
%% types of #tscell.xxx_value fields, constrained by what protobuf messages accept
%% -type pbvalue() :: binary() | integer() | boolean().
-export_type([ldbvalue/0]).


-spec encode_field_type(atom()) -> atom().
encode_field_type(binary) ->
    'BINARY';
encode_field_type(integer) ->
    'INTEGER';
encode_field_type(float) ->
    'NUMERIC';
encode_field_type(timestamp) ->
    'TIMESTAMP';
encode_field_type(boolean) ->
    'BOOLEAN';
encode_field_type(set) ->
    'SET';
encode_field_type(map) ->
    'MAP'.


%% TODO: actually support column specifiers
encode_columns(Columns) ->
    [#tscolumndescription{name = C} || C <- Columns].

decode_columns(Columns) ->
    [C || #tscolumndescription{name = C} <- Columns].


-spec encode_rows(list(list({binary(), ldbvalue()}))) -> [#tsrow{}].
%% @ignore copied from riakc_ts_put_operator; inverse of make_data
encode_rows(Measurements) ->
    rows_for(Measurements, []).

rows_for([], SerializedMeasurements) ->
    SerializedMeasurements;
rows_for([MeasureRow|RemainingMeasures], SerializedMeasurements) ->
    SerializedRow = row_for(MeasureRow),
    rows_for(RemainingMeasures, [SerializedRow | SerializedMeasurements]).

-spec row_for(list({binary(), ldbvalue()})) -> [#tscell{}].
row_for(MeasureRow) ->
    row_for(MeasureRow, []).

row_for([], SerializedCells) ->
    #tsrow{cells = lists:reverse(SerializedCells)};
row_for([Datum|RemainingCells], SerializedCells) ->
    row_for(RemainingCells,
            [cell_for(Datum) | SerializedCells]).

-spec cell_for(ldbvalue()) -> #tscell{}.
cell_for(Measure) when is_binary(Measure) ->
    #tscell{binary_value = Measure};
cell_for(Measure) when is_integer(Measure),
                       (?SINT64_MIN =< Measure),
                       (Measure =< ?SINT64_MAX)  ->
    #tscell{integer_value = Measure};
cell_for(Measure) when is_integer(Measure) ->
    #tscell{numeric_value = integer_to_list(Measure)};
cell_for(Measure) when is_float(Measure) ->
    #tscell{numeric_value = float_to_list(Measure)};
cell_for({time, Measure}) ->
    #tscell{timestamp_value = Measure};
cell_for(true) ->
    #tscell{boolean_value = true};
cell_for(false) ->
    #tscell{boolean_value = false};
%% and what about map and set?
%% TODO: is map a proplist?
cell_for(Measure) when is_list(Measure) andalso length(Measure) > 0 andalso
                       is_tuple(hd(Measure)) andalso size(hd(Measure)) == 2 ->
    #tscell{map_value = Measure};
cell_for(Measure) when is_list(Measure) ->
    #tscell{set_value = Measure}.



-spec decode_rows([#tsrow{}]) -> list(list(ldbvalue())).
decode_rows(Rows) ->
    decode_row(Rows, []).

-spec decode_row([#tsrow{}], list(ldbvalue())) -> [[ldbvalue()]].
decode_row([], Acc) ->
    lists:reverse(Acc);
decode_row([{tsrow, Row} | T], Acc) ->
    decode_row(T, [deccode_cell(Row, []) | Acc]).

-spec deccode_cell([#tscell{}], [ldbvalue()]) -> [ldbvalue()].
deccode_cell([], Acc) ->
    list_to_tuple(lists:reverse(Acc));
deccode_cell([#tscell{binary_value    = Bin,
                      integer_value   = undefined,
                      numeric_value   = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      set_value       = [],
                      map_value       = undefined} | T], Acc) ->
    deccode_cell(T, [Bin | Acc]);
deccode_cell([#tscell{binary_value    = undefined,
                      integer_value   = Int,
                      numeric_value   = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      set_value       = [],
                      map_value       = undefined} | T], Acc) ->
    deccode_cell(T, [Int | Acc]);
deccode_cell([#tscell{binary_value    = undefined,
                      integer_value   = undefined,
                      numeric_value   = Num,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      set_value       = [],
                      map_value       = undefined} | T], Acc) ->
    deccode_cell(T, [list_to_float(binary_to_list(Num)) | Acc]);
deccode_cell([#tscell{binary_value    = undefined,
                      integer_value   = undefined,
                      numeric_value   = undefined,
                      timestamp_value = Timestamp,
                      boolean_value   = undefined,
                      set_value       = [],
                      map_value       = undefined} | T], Acc) ->
    deccode_cell(T, [Timestamp | Acc]);
deccode_cell([#tscell{binary_value    = undefined,
                      integer_value   = undefined,
                      numeric_value   = undefined,
                      timestamp_value = undefined,
                      boolean_value   = Bool,
                      set_value       = [],
                      map_value       = undefined} | T], Acc) ->
    deccode_cell(T, [Bool | Acc]);
deccode_cell([#tscell{binary_value    = undefined,
                      integer_value   = undefined,
                      numeric_value   = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      set_value       = Set,
                      map_value       = undefined} | T], Acc) ->
    deccode_cell(T, [Set | Acc]);
deccode_cell([#tscell{binary_value    = undefined,
                      integer_value   = undefined,
                      numeric_value   = undefined,
                      timestamp_value = undefined,
                      boolean_value   = undefined,
                      set_value       = [],
                      map_value       = Map} | T], Acc) ->
    deccode_cell(T, [Map | Acc]).
