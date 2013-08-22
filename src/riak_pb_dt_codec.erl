%% -------------------------------------------------------------------
%%
%% riak_pb_dt_codec: Protocol Buffers utility functions for Riak DT types
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
-module(riak_pb_dt_codec).

-include("riak_dt_pb.hrl").

-export([
         encode_fetch_request/2,
         encode_fetch_request/3,
         decode_fetch_response/1,
         encode_fetch_response/3,
         encode_update_request/4,
         encode_update_request/5,
         decode_operation/1
        ]).

-import(riak_pb_kv_codec, [encode_quorum/1]).

-export_type([context/0]).

%% Value types
-type counter_value() :: integer().
-type set_value() :: [ binary() ].
-type register_value() :: binary().
-type flag_value() :: boolean().
-type map_entry() :: {map_field(), embedded_value()}.
-type map_field() :: {binary(), embedded_type()}.
-type map_value() :: [ map_entry() ].
-type embedded_value() :: counter_value() | set_value() | register_value() | flag_value().
-type toplevel_value() :: counter_value() | set_value() | map_value().
-opaque context() :: binary().

%% Type names as atoms
-type embedded_type() :: counter | set | register | flag.
-type toplevel_type() :: counter | set | map.

%% Operations
-type counter_op() :: increment | decrement | {increment | decrement, integer()}.
-type set_op() :: {add, binary() | [binary()]} | {remove, binary() | [binary()]}.
-type flag_op() :: enable | disable.
-type register_op() :: {assign, binary()}.
-type map_op() :: {add | remove, [map_field()]} | {update, map_field(), [embedded_type_op()]}.
-type embedded_type_op() :: counter_op() | set_op() | register_op() | flag_op().
-type toplevel_op() :: counter_op() | set_op() | map_op().

%% Request options
-type quorum() :: riak_pb_kv_codec:quorum().
-type update_opt() :: {w, quorum()} | {dw, quorum()} | {pw, quorum()} |
                      return_body | {return_body, boolean()} |
                      {timeout, pos_integer()} |
                      sloppy_quorum | {sloppy_quorum, boolean()} |
                      {n_val, pos_integer()}.
-type fetch_opt() :: {r, quorum()} | {pr, quorum()} |
                     basic_quorum | {basic_quorum, boolean()} |
                     notfound_ok | {notfound_ok, boolean()} |
                     {timeout, pos_integer()} |
                     sloppy_quorum | {sloppy_quorum, boolean()} |
                     {n_val, pos_integer()} |
                     include_context | {include_context, boolean()}.


%% @doc Decodes a MapField message into a tuple of name and type.
-spec decode_map_field(#mapfield{}) -> map_field().
decode_map_field(#mapfield{name=Name,type=Type}) ->
    {Name, decode_type(Type)}.

%% @doc Encodes a tuple of name and type into a MapField message.
-spec encode_map_field(map_field()) -> #mapfield{}.
encode_map_field({Name, Type}) ->
    #mapfield{name=Name, type=encode_type(Type)}.

%% @doc Decodes an MapEntry message into a tuple of field and value.
-spec decode_map_entry(#mapentry{}) -> map_entry().
decode_map_entry(#mapentry{field=#mapfield{type='COUNTER'}=Field, counter_value=Val}) ->
    {decode_map_field(Field), Val};
decode_map_entry(#mapentry{field=#mapfield{type='SET'}=Field, set_value=Val}) ->
    {decode_map_field(Field), Val};
decode_map_entry(#mapentry{field=#mapfield{type='REGISTER'}=Field, register_value=Val}) ->
    {decode_map_field(Field), Val};
decode_map_entry(#mapentry{field=#mapfield{type='FLAG'}=Field, flag_value=Val}) ->
    {decode_map_field(Field), Val}.

%% @doc Encodes a tuple of field and value into a MapEntry message.
-spec encode_map_entry(map_entry()) -> #mapentry{}.
encode_map_entry({{Name, counter=Type}, Value}) when is_integer(Value) ->
    #mapentry{field=encode_map_field({Name, Type}), counter_value=Value};
encode_map_entry({{Name, set=Type}, Value}) when is_list(Value) ->
    #mapentry{field=encode_map_field({Name, Type}), set_value=Value};
encode_map_entry({{Name, register=Type}, Value}) when is_binary(Value) ->
    #mapentry{field=encode_map_field({Name, Type}), register_value=Value};
encode_map_entry({{Name, flag=Type}, Value}) when is_boolean(Value) ->
    #mapentry{field=encode_map_field({Name, Type}), flag_value=Value}.


%% @doc Encodes a fetch request into a DtFetch message.
-spec encode_fetch_request({binary(), binary()}, binary()) -> #dtfetch{}.
encode_fetch_request(BucketAndType, Key) ->
    encode_fetch_request(BucketAndType, Key, []).

-spec encode_fetch_request({binary(), binary()}, binary(), [fetch_opt()]) -> #dtfetch{}.
encode_fetch_request({BType,Bucket}, Key, Options) ->
    encode_fetch_options(#dtfetch{bucket=Bucket,key=Key,type=BType}, Options).

%% @doc Encodes request-time fetch options onto the DtFetch message.
%% @private
-spec encode_fetch_options(#dtfetch{}, [fetch_opt()]) -> #dtfetch{}.
encode_fetch_options(Fetch, []) ->
    Fetch;
encode_fetch_options(Fetch, [{r,R}|Tail]) ->
    encode_fetch_options(Fetch#dtfetch{r=encode_quorum(R)},Tail);
encode_fetch_options(Fetch, [{pr,PR}|Tail]) ->
    encode_fetch_options(Fetch#dtfetch{pr=encode_quorum(PR)},Tail);
encode_fetch_options(Fetch, [basic_quorum|Tail]) ->
    encode_fetch_options(Fetch, [{basic_quorum, true}|Tail]);
encode_fetch_options(Fetch, [{basic_quorum, BQ}|Tail]) ->
    encode_fetch_options(Fetch#dtfetch{basic_quorum=BQ},Tail);
encode_fetch_options(Fetch, [notfound_ok|Tail]) ->
    encode_fetch_options(Fetch, [{notfound_ok, true}|Tail]);
encode_fetch_options(Fetch, [{notfound_ok, NOK}|Tail]) ->
    encode_fetch_options(Fetch#dtfetch{notfound_ok=NOK},Tail);
encode_fetch_options(Fetch, [{timeout, TO}|Tail]) ->
    encode_fetch_options(Fetch#dtfetch{timeout=TO},Tail);
encode_fetch_options(Fetch, [sloppy_quorum|Tail]) ->
    encode_fetch_options(Fetch, [{sloppy_quorum, true}|Tail]);
encode_fetch_options(Fetch, [{sloppy_quorum, RB}|Tail]) ->
    encode_fetch_options(Fetch#dtfetch{sloppy_quorum=RB},Tail);
encode_fetch_options(Fetch, [{n_val, N}|Tail]) ->
    encode_fetch_options(Fetch#dtfetch{n_val=N}, Tail);
encode_fetch_options(Fetch, [include_context|Tail]) ->
    encode_fetch_options(Fetch, [{include_context, true}|Tail]);
encode_fetch_options(Fetch, [{include_context, IC}|Tail]) ->
    encode_fetch_options(Fetch#dtfetch{include_context=IC},Tail);
encode_fetch_options(Fetch, [_|Tail]) ->
    encode_fetch_options(Fetch, Tail).

%% @doc Decodes a FetchResponse into tuple of type, value and context.
-spec decode_fetch_response(#dtfetchresponse{}) -> {toplevel_type(), toplevel_value(), context()}.
decode_fetch_response(#dtfetchresponse{context=Context, type='COUNTER', counter_value=Val}) ->
    {counter, Val, Context};
decode_fetch_response(#dtfetchresponse{context=Context, type='SET', set_value=Val}) ->
    {set, Val, Context};
decode_fetch_response(#dtfetchresponse{context=Context, type='MAP', map_value=Val}) ->
    {map, [ decode_map_entry(Entry) || Entry <- Val ], Context}.

%% @doc Encodes the result of a fetch request into a FetchResponse message.
-spec encode_fetch_response(toplevel_type(), toplevel_value(), context()) -> #dtfetchresponse{}.
encode_fetch_response(Type, undefined, _Context) ->
    %% TODO: "Not found" may be undefined, or it may be the
    %% bottom-value of the type, but we need to send something back.
    %% There is also no "context" for a missing datatype, or its
    %% bottom-value.
    #dtfetchresponse{type=encode_type(Type)};
encode_fetch_response(Type, Value, Context) ->
    Response = #dtfetchresponse{context=Context, type=encode_type(Type)},
    case Type of
        counter ->
            Response#dtfetchresponse{counter_value=Value};
        set ->
            Response#dtfetchresponse{set_value=Value};
        map ->
            Response#dtfetchresponse{map_value=[encode_map_entry(Entry) || Entry <- Value]}
    end.

%% @doc Decodes a DtOperation message into a datatype-specific operation.
-spec decode_operation(#dtoperation{}) -> toplevel_op().
decode_operation(#dtoperation{counter_op=#counterop{}=Op}) ->
    decode_counter_op(Op);
decode_operation(#dtoperation{set_op=#setop{}=Op}) ->
    decode_set_op(Op);
decode_operation(#dtoperation{map_op=#mapop{op='UPDATE', update_field=Field, field_ops=Ops}}) ->
    {_,FType} = DecodedField = decode_map_field(Field),
    {update, DecodedField, [decode_map_field_op(MOp, FType) || MOp <- Ops]};
decode_operation(#dtoperation{map_op=#mapop{op=Op, add_remove_fields=Fields}}) ->
    {decode_op_type(Op), [ decode_map_field(Field) || Field <- Fields ]}.

%% @doc Encodes a datatype-specific operation into a DtOperation message.
-spec encode_operation(toplevel_op(), toplevel_type()) -> #dtoperation{}.
encode_operation(Op, counter) ->
    #dtoperation{counter_op=encode_counter_op(Op)};
encode_operation(Op, set) ->
    #dtoperation{set_op=encode_set_op(Op)};
encode_operation({update, {_,Type}=Field, FieldOps}, map) ->
    MapOp = #mapop{
               op='UPDATE',
               update_field=encode_map_field(Field),
               field_ops=[ encode_map_field_op(Op,Type) || Op <- FieldOps ]
              },
    #dtoperation{map_op=MapOp};
encode_operation({Op, Fields}, map) when add == Op orelse remove == Op ->
    #dtoperation{map_op=#mapop{op=encode_op_type(Op),
                               add_remove_fields=Fields}}.


%% @doc Decodes a MapFieldOp message into a datatype-specific operation.
-spec decode_map_field_op(#mapfieldop{}, embedded_type()) -> embedded_type_op().
decode_map_field_op(#mapfieldop{counter_op=Op}, counter) ->
    decode_counter_op(Op);
decode_map_field_op(#mapfieldop{set_op=Op}, set) ->
    decode_set_op(Op);
decode_map_field_op(#mapfieldop{register_op=Value}, register) ->
    {assign, Value};
decode_map_field_op(#mapfieldop{flag_op=Op}, flag) ->
    decode_op_type(Op).

%% @doc Encodes a datatype-specific operation into a MapFieldOp message.
-spec encode_map_field_op(embedded_type_op(), embedded_type()) -> #mapfieldop{}.
encode_map_field_op(Op, counter) ->
    #mapfieldop{counter_op=encode_counter_op(Op)};
encode_map_field_op(Op, set) ->
    #mapfieldop{set_op=encode_set_op(Op)};
encode_map_field_op({set, Value}, register) ->
    #mapfieldop{register_op=Value};
encode_map_field_op(Op, flag) ->
    #mapfieldop{flag_op=encode_op_type(Op)}.

%% @doc Decodes a CounterOp message into a counter operation.
-spec decode_counter_op(#counterop{}) -> counter_op().
decode_counter_op(#counterop{increment=Int}) when is_integer(Int) ->
    {increment, Int};
decode_counter_op(#counterop{increment=undefined}) ->
    increment.

%% @doc Encodes a counter operation into a CounterOp message.
-spec encode_counter_op(counter_op()) -> #counterop{}.
encode_counter_op({increment, Int}) when is_integer(Int) ->
    #counterop{increment=Int};
encode_counter_op(increment) ->
    #counterop{};
encode_counter_op(decrement) ->
    #counterop{increment=-1};
encode_counter_op({decrement, Int}) when is_integer(Int) ->
    #counterop{increment=(-Int)}.

%% @doc Decodes a SetOp message into a set operation.
-spec decode_set_op(#setop{}) -> set_op().
decode_set_op(#setop{op=Op, member=Member}) ->
    {decode_op_type(Op), Member}.

%% @doc Encodes a set operation into a SetOp message.
-spec encode_set_op(set_op()) -> #setop{}.
encode_set_op({Op, Member}) when is_binary(Member) ->
    encode_set_op({Op, [Member]});
encode_set_op({Op, Members}) when is_list(Members) andalso (add == Op orelse remove == Op) ->
    #setop{op=encode_op_type(Op), member=Members}.

%% @doc Decodes a operation name from a PB message into an atom.
-spec decode_op_type(atom()) -> atom().
decode_op_type('ENABLE')  -> enable;
decode_op_type('DISABLE') -> disable;
decode_op_type('ADD')     -> add;
decode_op_type('REMOVE')  -> remove;
decode_op_type('UPDATE')  -> update.

%% @doc Encodes an atom operation name into the PB message equivalent.
-spec encode_op_type(atom()) -> atom().
encode_op_type(enable)  -> 'ENABLE';
encode_op_type(disable) -> 'DISABLE';
encode_op_type(add)     -> 'ADD';
encode_op_type(remove)  -> 'REMOVE';
encode_op_type(update)  -> 'UPDATE'.

%% @doc Decodes a PB message type name into an atom.
-spec decode_type(atom()) -> atom().
decode_type('COUNTER')  -> counter;
decode_type('SET')      -> set;
decode_type('REGISTER') -> register;
decode_type('FLAG')     -> flag;
decode_type('MAP')      -> map.

%% @doc Encodes an atom type name into the PB message equivalent.
-spec encode_type(atom()) -> atom().
encode_type(counter)  -> 'COUNTER';
encode_type(set)      -> 'SET';
encode_type(register) -> 'REGISTER';
encode_type(flag)     -> 'FLAG';
encode_type(map)      -> 'MAP'.

%% @doc Encodes an update request into a DtUpdate message.
-spec encode_update_request({binary(), binary()}, binary() | undefined, toplevel_type(), [toplevel_op()]) -> #dtupdate{}.
encode_update_request({_,_}=BucketAndType, Key, Type, Ops) ->
    encode_update_request(BucketAndType, Key, Type, Ops, []).

-spec encode_update_request({binary(), binary()}, binary() | undefined, toplevel_type(), [toplevel_op()], [update_opt()]) -> #dtupdate{}.
encode_update_request({BType, Bucket}, Key, DType, Ops, Options) ->
    Update = #dtupdate{bucket=Bucket,
                       key=Key,
                       type=BType,
                       ops=[encode_operation(Op, DType) || Op <- Ops]},
    encode_update_options(Update, Options).

%% @doc Encodes request-time update options onto the DtUpdate message.
%% @private
-spec encode_update_options(#dtupdate{}, [proplists:property()]) -> #dtupdate{}.
encode_update_options(Update, []) ->
    Update;
encode_update_options(Update, [{w,W}|Tail]) ->
    encode_update_options(Update#dtupdate{w=encode_quorum(W)},Tail);
encode_update_options(Update, [{dw,DW}|Tail]) ->
    encode_update_options(Update#dtupdate{dw=encode_quorum(DW)},Tail);
encode_update_options(Update, [{pw,PW}|Tail]) ->
    encode_update_options(Update#dtupdate{pw=encode_quorum(PW)},Tail);
encode_update_options(Update, [return_body|Tail]) ->
    encode_update_options(Update, [{return_body, true}|Tail]);
encode_update_options(Update, [{return_body, RB}|Tail]) ->
    encode_update_options(Update#dtupdate{return_body=RB},Tail);
encode_update_options(Update, [{timeout, TO}|Tail]) ->
    encode_update_options(Update#dtupdate{timeout=TO},Tail);
encode_update_options(Update, [sloppy_quorum|Tail]) ->
    encode_update_options(Update, [{sloppy_quorum, true}|Tail]);
encode_update_options(Update, [{sloppy_quorum, RB}|Tail]) ->
    encode_update_options(Update#dtupdate{sloppy_quorum=RB},Tail);
encode_update_options(Update, [{n_val, N}|Tail]) ->
    encode_update_options(Update#dtupdate{n_val=N}, Tail);
encode_update_options(Update, [_|Tail]) ->
    encode_update_options(Update, Tail).
