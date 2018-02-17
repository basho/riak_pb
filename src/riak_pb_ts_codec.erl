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

-export([encode_columnnames/1,
         encode_rows/2,
         encode_rows_non_strict/1,
         encode_columns/2,
         decode_rows/1,
         encode_cells/1,
         encode_cells_non_strict/1,
         decode_cells/1,
         encode_field_type/1,
         encode_cover_list/1,
         decode_cover_list/1]).


-type tsrow() :: #'TsRow'{}.
-export_type([tsrow/0]).

%% types existing between us and eleveldb
-type ldbvalue() :: binary() | number() | boolean() | list().

%% types of #'TsCell'.xxx_value fields, constrained by what protobuf messages accept
%% -type pbvalue() :: binary() | integer() | boolean().
-export_type([ldbvalue/0]).

%% Column names are binary only.
-type tscolumnname() :: binary().
%% Possible column type values supported and returned from the timeseries DDL.
-type tscolumntype() :: varchar | sint64 | timestamp | boolean | double.
%% Possible column type values that protocol buffers supports for enumeration purposes.
-type tscolumntypePB() :: 'VARCHAR' | 'SINT64' | 'TIMESTAMP' | 'BOOLEAN' | 'DOUBLE'.
-export_type([tscolumnname/0, tscolumntype/0, tscolumntypePB/0]).

%% @doc Convert a list of column names to partial #'TsColumnDescription' records.
-spec encode_columnnames([tscolumnname()]) -> [#'TsColumnDescription'{}].
encode_columnnames(ColumnNames) ->
    [#'TsColumnDescription'{name = C} || C <- ColumnNames].

%% @doc Convert time series field type atoms returned from the DDL modules
%% into Protobuf compatible upper case atoms.
-spec encode_field_type(tscolumntype()) -> atom().
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

%% @doc Encode a set of time series rows from an internal format to the #'TsRow' record format.
%% Takes a list of column types, and a list of rows.
%% Each row is represented as a list of ldbvalue().
%% An error is returned if any of the `Rows` individual row length do not match the length of the `ColumnTypes` list.
%% @end
-spec encode_rows([tscolumntype()], [{ldbvalue()}] | [[ldbvalue()]]) -> [#'TsRow'{}].
encode_rows(ColumnTypes, Rows) ->
    [encode_row(ColumnTypes, Row) || Row <- Rows].

%% @doc Only for encoding rows for PUTs on the erlang client.
%%      Will not properly encode timestamp #'TsCell'{} records,
%%      but this is OK for this use case since the values
%%      get cast in the order of:
%%      lvldbvalue() -> #'TsCell'{} -> lvldbvalue().
%%      THEREFORE no info is lost for these cases.
%% @end
encode_rows_non_strict(Rows) ->
    [encode_row_non_strict(Row) || Row <- Rows].

-spec encode_columns([binary()], [riak_pb_ts_codec:tscolumntype()]) ->
                                           [#'TsColumnDescription'{}].
encode_columns(ColumnNames, ColumnTypes) ->
    [#'TsColumnDescription'{name = Name, type = encode_field_type(Type)}
     || {Name, Type} <- lists:zip(ColumnNames, ColumnTypes)].


%% @doc Decode a list of timeseries #'TsRow'{} to a list of tuples.
%% Each row is converted through `decode_cells/1`, and the list
%% of ldbvalue() is converted to a tuple of ldbvalue().
%% @end
-spec decode_rows([#'TsRow'{}]) -> [{ldbvalue()}].
decode_rows(Rows) ->
    [list_to_tuple(decode_cells(Cells)) || #'TsRow'{cells = Cells} <- Rows].

-spec encode_cells([{tscolumntype(), ldbvalue()}]) -> [#'TsCell'{}].
encode_cells(Cells) ->
    [encode_cell(C) || C <- Cells].

%% @doc Decode a list of timeseries #'TsCell'{} to a list of ldbvalue().
-spec decode_cells([#'TsCell'{}]) -> [ldbvalue()].
decode_cells(Cells) ->
    decode_cells(Cells, []).

%% ---------------------------------------
%% local functions
%% ---------------------------------------

-spec encode_row([tscolumntype()], [ldbvalue()] | {ldbvalue()}) -> #'TsRow'{}.
encode_row(ColumnTypes, RowCells) when is_tuple(RowCells) ->
    encode_row(ColumnTypes, tuple_to_list(RowCells));
encode_row(ColumnTypes, RowCells) when is_list(RowCells), length(ColumnTypes) =:= length(RowCells) ->
    #'TsRow'{cells = [encode_cell(ColumnTypeCell) ||
                    ColumnTypeCell <- lists:zip(ColumnTypes, RowCells)]}.

%% @doc Only for encoding rows for PUTs on the erlang client.
%%      Will not properly encode timestamp #'TsCell'{} records,
%%      but this is OK for these use cases since the values
%%      get cast in the order of:
%%      lvldbvalue() -> #'TsCell'{} -> lvldbvalue().
%%      THEREFORE no info is lost for these cases.
%% @end
-spec encode_row_non_strict([ldbvalue()]) -> #'TsRow'{}.
encode_row_non_strict(RowCells) ->
    #'TsRow'{cells = encode_cells_non_strict(RowCells)}.

%% @doc Only for encoding cells for PUTs on the erlang client,
%%      and Key cells for get / delete requests.
%%      Will not properly encode timestamp #'TsCell'{} records,
%%      but this is OK for these use cases since the values
%%      get cast in the order of:
%%      lvldbvalue() -> #'TsCell'{} -> lvldbvalue().
%%      THEREFORE no info is lost for these cases.
%% @end
-spec encode_cells_non_strict([ldbvalue()] | {ldbvalue()}) -> [#'TsCell'{}].
encode_cells_non_strict(Cells) when is_tuple(Cells) ->
    encode_cells_non_strict(tuple_to_list(Cells));
encode_cells_non_strict(Cells) when is_list(Cells) ->
    [encode_cell_non_strict(Cell) || Cell <- Cells].

-spec encode_cell({tscolumntype(), ldbvalue()}) -> #'TsCell'{}.
encode_cell({varchar, V}) when is_binary(V) ->
    #'TsCell'{varchar_value = V};
encode_cell({sint64, V}) when is_integer(V) ->
    #'TsCell'{sint64_value = V};
encode_cell({double, V}) when is_float(V) ->
    #'TsCell'{double_value = V};
encode_cell({timestamp, V}) when is_integer(V) ->
    #'TsCell'{timestamp_value = V};
encode_cell({boolean, V}) when is_boolean(V) ->
    #'TsCell'{boolean_value = V};
encode_cell({_ColumnType, undefined}) ->
    #'TsCell'{};
%% NULL Cell
%% TODO: represent null cells by something other than an empty list. emptyTsCell atom maybe?
encode_cell({_ColumnType, []}) ->
    #'TsCell'{}.

%% @doc Only for encoding rows for PUTs on the erlang client,
%%      and Key cells for get / delete requests.
%%      Will not properly encode timestamp #'TsCell'{} records,
%%      but this is OK for these use cases since the values
%%      get cast in the order of:
%%      lvldbvalue() -> #'TsCell'{} -> lvldbvalue().
%%      THEREFORE no info is lost for these cases.
%% @end
-spec encode_cell_non_strict(ldbvalue()) -> #'TsCell'{}.
encode_cell_non_strict(V) when is_binary(V) ->
    #'TsCell'{varchar_value = V};
encode_cell_non_strict(V) when is_integer(V) ->
    #'TsCell'{sint64_value = V};
encode_cell_non_strict(V) when is_float(V) ->
    #'TsCell'{double_value = V};
encode_cell_non_strict(V) when is_boolean(V) ->
    #'TsCell'{boolean_value = V};
encode_cell_non_strict(undefined) ->
    #'TsCell'{};
%% NULL Cell
%% TODO: represent null cells by something other than an empty list. emptyTsCell atom maybe?
encode_cell_non_strict([]) ->
    #'TsCell'{}.

-spec decode_cells([#'TsCell'{}], [ldbvalue()]) -> [ldbvalue()].
decode_cells([], Acc) ->
    lists:reverse(Acc);
decode_cells([#'TsCell'{varchar_value = Bin,
    sint64_value = undefined,
    timestamp_value = undefined,
    boolean_value = undefined,
    double_value = undefined} | T], Acc)
    when is_binary(Bin) ->
    decode_cells(T, [Bin | Acc]);
decode_cells([#'TsCell'{varchar_value = undefined,
    sint64_value = Int,
    timestamp_value = undefined,
    boolean_value = undefined,
    double_value = undefined} | T], Acc)
    when is_integer(Int) ->
    decode_cells(T, [Int | Acc]);
decode_cells([#'TsCell'{varchar_value = undefined,
    sint64_value = undefined,
    timestamp_value = Timestamp,
    boolean_value = undefined,
    double_value = undefined} | T], Acc)
    when is_integer(Timestamp) ->
    decode_cells(T, [Timestamp | Acc]);
decode_cells([#'TsCell'{varchar_value = undefined,
    sint64_value = undefined,
    timestamp_value = undefined,
    boolean_value = Bool,
    double_value = undefined} | T], Acc)
    when is_boolean(Bool) ->
    decode_cells(T, [Bool | Acc]);
decode_cells([#'TsCell'{varchar_value = undefined,
    sint64_value = undefined,
    timestamp_value = undefined,
    boolean_value = undefined,
    double_value = Double} | T], Acc)
    when is_float(Double) ->
    decode_cells(T, [Double | Acc]);
decode_cells([#'TsCell'{varchar_value = undefined,
    sint64_value = undefined,
    timestamp_value = undefined,
    boolean_value = undefined,
    double_value = undefined} | T], Acc) ->
    %% NULL Cell.
    %% TODO: represent null cells by something other than an empty list. emptyTsCell atom maybe?
    decode_cells(T, [[] | Acc]).



%% Copied and modified from riak_kv_pb_coverage:convert_list. Would
%% be nice to collapse them back together, probably with a closure,
%% but time and effort.
-type ts_range() :: {FieldName::binary(),
                     {{StartVal::integer(), StartIncl::boolean()},
                      {EndVal::integer(), EndIncl::boolean()}}}.

-spec encode_cover_list([{{IP::string(), Port::non_neg_integer()},
                          Context::binary(),
                          ts_range(),
                          SQLText::binary()}]) -> [#'TsCoverageEntry'{}].
encode_cover_list(Entries) ->
    [#'TsCoverageEntry'{ip = IP, port = Port,
                      cover_context = Context,
                      range = encode_ts_range({Range, SQLText})}
     || {{IP, Port}, Context, Range, SQLText} <- Entries].

-spec decode_cover_list([#'TsCoverageEntry'{}]) ->
                               [{{IP::string(), Port::non_neg_integer()},
                                 CoverContext::binary(), ts_range(), Text::binary()}].
decode_cover_list(Entries) ->
    [begin
         {RangeStruct, Text} = decode_ts_range(Range),
         {{IP, Port}, CoverContext, RangeStruct, Text}
     end || #'TsCoverageEntry'{ip = IP, port = Port,
                             cover_context = CoverContext,
                             range = Range} <- Entries].

-spec encode_ts_range({ts_range(), binary()}) -> #'TsRange'{}.
encode_ts_range({{FieldName, {{StartVal, StartIncl}, {EndVal, EndIncl}}}, Text}) ->
    #'TsRange'{field_name            = FieldName,
             lower_bound           = StartVal,
             lower_bound_inclusive = StartIncl,
             upper_bound           = EndVal,
             upper_bound_inclusive = EndIncl,
             desc                  = Text
            }.

-spec decode_ts_range(#'TsRange'{}) -> {ts_range(), binary()}.
decode_ts_range(#'TsRange'{field_name            = FieldName,
                         lower_bound           = StartVal,
                         lower_bound_inclusive = StartIncl,
                         upper_bound           = EndVal,
                         upper_bound_inclusive = EndIncl,
                         desc                  = Text}) ->
    {{FieldName, {{StartVal, StartIncl}, {EndVal, EndIncl}}}, Text}.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

encode_cells_test() ->
    %% Correct cells
    ?assertEqual(#'TsCell'{varchar_value = <<"Foo">>}, encode_cell({varchar, <<"Foo">>})),
    ?assertEqual(#'TsCell'{sint64_value = 64}, encode_cell({sint64, 64})),
    ?assertEqual(#'TsCell'{timestamp_value = 64}, encode_cell({timestamp, 64})),
    ?assertEqual(#'TsCell'{boolean_value = true}, encode_cell({boolean, true})),
    ?assertEqual(#'TsCell'{double_value = 42.0}, encode_cell({double, 42.0})),
    ?assertEqual(#'TsCell'{boolean_value = false}, encode_cell({boolean, false})),

    %% Null Cells
    ?assertEqual(#'TsCell'{}, encode_cell({varchar, []})),
    ?assertEqual(#'TsCell'{}, encode_cell({sint64, []})),
    ?assertEqual(#'TsCell'{}, encode_cell({timestamp, []})),
    ?assertEqual(#'TsCell'{}, encode_cell({boolean, []})),
    ?assertEqual(#'TsCell'{}, encode_cell({double, []})),

    %% Just plain wrong Cells
    ?assertError(function_clause, encode_cell({varchar, 42})),
    ?assertError(function_clause, encode_cell({varchar, true})),
    ?assertError(function_clause, encode_cell({sint64, <<"42">>})),
    ?assertError(function_clause, encode_cell({sint64, true})),
    ?assertError(function_clause, encode_cell({boolean, <<"42">>})),
    ?assertError(function_clause, encode_cell({boolean, 42})).

encode_row_test() ->
    ?assertEqual(
        #'TsRow'{cells = [
            #'TsCell'{varchar_value = <<"Foo">>},
            #'TsCell'{sint64_value = 64},
            #'TsCell'{timestamp_value = 42},
            #'TsCell'{boolean_value = false},
            #'TsCell'{double_value = 42.2},
            #'TsCell'{}
        ]},
        encode_row(
            [varchar, sint64, timestamp, boolean, double, varchar],
            [<<"Foo">>, 64, 42, false, 42.2, []]
        )),
    ?assertError(function_clause, encode_row([], [<<"Foo">>, 64, 42, false, 42.2, []])),
    ?assertError(function_clause, encode_row([varchar, sint64, timestamp], [<<"Foo">>, 64, 42, false, 42.2, []])).

encode_rows_test() ->
    ?assertEqual(
        [
            #'TsRow'{cells = [
                #'TsCell'{varchar_value = <<"Foo">>},
                #'TsCell'{sint64_value = 30}
            ]},
            #'TsRow'{cells = [
                #'TsCell'{varchar_value = <<"Bar">>},
                #'TsCell'{sint64_value = 40}
            ]}
        ],
        encode_rows([varchar, sint64], [[<<"Foo">>, 30], [<<"Bar">>, 40]])
    ),
    ?assertError(function_clause, encode_rows([], [[<<"Foo">>, 30], [<<"Bar">>, 40]])),
    ?assertError(function_clause, encode_rows([varchar], [[<<"Foo">>, 30], [<<"Bar">>, 40]])).

encode_field_type_test() ->
    ?assertEqual('VARCHAR', encode_field_type(varchar)),
    ?assertEqual('SINT64',encode_field_type(sint64)),
    ?assertEqual('TIMESTAMP',encode_field_type(timestamp)),
    ?assertEqual('BOOLEAN',encode_field_type(boolean)),
    ?assertEqual('DOUBLE',encode_field_type(double)).

decode_cell_test() ->
    ?assertEqual([<<"Foo">>], decode_cells([#'TsCell'{varchar_value = <<"Foo">>}],[])),
    ?assertEqual([42],      decode_cells([#'TsCell'{sint64_value = 42}],[])),
    ?assertEqual([64],      decode_cells([#'TsCell'{timestamp_value = 64}],[])),
    ?assertEqual([false],   decode_cells([#'TsCell'{boolean_value = false}],[])),
    ?assertEqual([42.2],    decode_cells([#'TsCell'{double_value = 42.2}],[])),
    ?assertEqual([[]],      decode_cells([#'TsCell'{}],[])),
    ?assertEqual(
        [<<"Bar">>, 80, []],
        decode_cells([#'TsCell'{varchar_value = <<"Bar">>}, #'TsCell'{sint64_value = 80}, #'TsCell'{}],[])),
    ?assertError(
        function_clause,
        decode_cells([#'TsCell'{varchar_value = <<"Foo">>, sint64_value = 30}],[])).

decode_cells_test() ->
    ?assertEqual(
        [<<"Bar">>, 80, []],
        decode_cells([#'TsCell'{varchar_value = <<"Bar">>}, #'TsCell'{sint64_value = 80}, #'TsCell'{}])),
    ?assertError(
        function_clause,
        decode_cells([#'TsCell'{varchar_value = <<"Foo">>, sint64_value = 30}])).

decode_rows_test() ->
    ?assertEqual(
        [{<<"Bar">>, 80, []}, {<<"Baz">>, 90, false}],
        decode_rows([
            #'TsRow'{cells = [#'TsCell'{varchar_value = <<"Bar">>}, #'TsCell'{sint64_value = 80}, #'TsCell'{}]},
            #'TsRow'{cells = [#'TsCell'{varchar_value = <<"Baz">>}, #'TsCell'{sint64_value = 90}, #'TsCell'{boolean_value = false}]}])).

-endif.
