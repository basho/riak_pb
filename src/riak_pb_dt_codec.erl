%% -------------------------------------------------------------------
%%
%% riak_pb_dt_codec: Protocol Buffers utility functions for Riak DT types
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
-module(riak_pb_dt_codec).

-include("riak_dt_pb.hrl").

-export([
         encode_fetch_request/2,
         encode_fetch_request/3,
         decode_fetch_response/1,
         encode_fetch_response/3,
         encode_fetch_response/4,
         encode_update_request/3,
         encode_update_request/4,
         decode_operation/1,
         decode_operation/2,
         operation_type/1,
         decode_update_response/3,
         encode_update_response/4,
         encode_update_response/5,
         encode_operation/2
        ]).

-import(riak_pb_kv_codec, [encode_quorum/1]).

-export_type([context/0]).

%% Value types
-type context() :: binary() | undefined.
-type counter_value() :: integer().
-type set_value() :: [ binary() ].
-type hll_value() :: number().
-type register_value() :: binary().
-type flag_value() :: boolean().
-type map_entry() :: {map_field(), embedded_value()}.
-type map_field() :: {binary(), embedded_type()}.
-type map_value() :: [ map_entry() ].
-type embedded_value() :: counter_value() | set_value() | register_value()
                        | flag_value() | map_value().
-type toplevel_value() :: counter_value() | set_value() | map_value()
                          | hll_value() | undefined.
-type fetch_response() :: {toplevel_type(), toplevel_value(), context()}.

%% Type names as atoms
-type embedded_type() :: counter | set | register | flag | map.
-type toplevel_type() :: counter | set | map | hll.
-type all_type()      :: toplevel_type() | embedded_type().

%% Operations
-type counter_op() :: increment | decrement | {increment | decrement, integer()}.
-type simple_set_op() :: {add, binary()} | {remove, binary()} | {add_all, [binary()]} | {remove_all, [binary()]}.
-type set_op() :: simple_set_op() | {update, [simple_set_op()]}.
-type hll_op() :: {add, binary()} | {add_all, [binary()]}.
-type flag_op() :: enable | disable.
-type register_op() :: {assign, binary()}.
-type simple_map_op() :: {remove, map_field()} | {update, map_field(), embedded_type_op()}.
-type map_op() :: simple_map_op() | {update, [simple_map_op()]}.
-type embedded_type_op() :: counter_op() | set_op() | register_op() | flag_op() | map_op().
-type toplevel_op() :: counter_op() | set_op() | map_op() | hll_op().
-type update() :: {toplevel_type(), toplevel_op(), context()}.

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

%% Server-side type<->module mappings
-type type_mappings() :: [{all_type(), module()}].


%% =========================
%% DATA STRUCTURES AND TYPES
%% =========================

%% @doc Decodes a MapField message into a tuple of name and type.
-spec decode_map_field(#'MapField'{}, type_mappings()) -> map_field().
decode_map_field(#'MapField'{name=Name,type=Type}, Mods) ->
    {Name, decode_type(Type, Mods)}.

%% @doc Encodes a tuple of name and type into a MapField message.
-spec encode_map_field(map_field()) -> #'MapField'{}.
encode_map_field(Field) ->
    encode_map_field(Field, []).

%% @doc Encodes a tuple of name and type into a MapField message,
%% using the given type mappings.
-spec encode_map_field(map_field(), type_mappings()) -> #'MapField'{}.
encode_map_field({Name, Type}, Mods) ->
    #'MapField'{name=Name, type=encode_type(Type, Mods)}.

%% @doc Decodes an MapEntry message into a tuple of field and value.
-spec decode_map_entry(#'MapEntry'{}) -> map_entry().
decode_map_entry(Entry) ->
    decode_map_entry(Entry, []).

%% @doc Decodes an MapEntry message into a tuple of field and value,
%% using the given type mappings.
-spec decode_map_entry(#'MapEntry'{}, type_mappings()) -> map_entry().
decode_map_entry(#'MapEntry'{field=#'MapField'{type='COUNTER'}=Field, counter_value=Val}, Mods) ->
    {decode_map_field(Field, Mods), Val};
decode_map_entry(#'MapEntry'{field=#'MapField'{type='SET'}=Field, set_value=Val}, Mods) ->
    {decode_map_field(Field, Mods), Val};
decode_map_entry(#'MapEntry'{field=#'MapField'{type='REGISTER'}=Field, register_value=Val}, Mods) ->
    {decode_map_field(Field, Mods), Val};
decode_map_entry(#'MapEntry'{field=#'MapField'{type='FLAG'}=Field, flag_value=Val}, Mods) ->
    {decode_map_field(Field, Mods), Val};
decode_map_entry(#'MapEntry'{field=#'MapField'{type='MAP'}=Field, map_value=Val}, Mods) ->
    {decode_map_field(Field, Mods), [ decode_map_entry(Entry, Mods) || Entry <- Val ]}.


%% @doc Encodes a tuple of field and value into a MapEntry message.
-spec encode_map_entry(map_entry(), type_mappings()) -> #'MapEntry'{}.
encode_map_entry({{Name, counter=Type}, Value}, _Mods) when is_integer(Value) ->
    #'MapEntry'{field=encode_map_field({Name, Type}), counter_value=Value};
encode_map_entry({{Name, set=Type}, Value}, _Mods) when is_list(Value) ->
    #'MapEntry'{field=encode_map_field({Name, Type}), set_value=Value};
encode_map_entry({{Name, register=Type}, Value}, _Mods) when is_binary(Value) ->
    #'MapEntry'{field=encode_map_field({Name, Type}), register_value=Value};
encode_map_entry({{Name, register=Type}, undefined}, _Mods)  ->
    #'MapEntry'{field=encode_map_field({Name, Type})};
encode_map_entry({{Name, flag=Type}, Value}, _Mods) when is_atom(Value) ->
    #'MapEntry'{field=encode_map_field({Name, Type}), flag_value=encode_flag_value(Value)};
encode_map_entry({{Name, map=Type}, Value}, Mods) when is_list(Value) ->
    #'MapEntry'{field=encode_map_field({Name, Type}),
              map_value=[ encode_map_entry(Entry, Mods) || Entry <- Value ]};
encode_map_entry({{Name, Type}, Value}, Mods) ->
    %% We reach this clause if the type is not in the shortname yet,
    %% but is a module name.
    case lists:keyfind(Type, 2, Mods) of
        false ->
            %% If you don't have a mapping, we can't encode it.
            erlang:error(badarg, [{{Name,Type},Value}, Mods]);
        {AtomType, Type} ->
            encode_map_entry({{Name,AtomType}, Value}, Mods)
    end.

%% @doc Decodes a PB message type name into a module name according to
%% the passed mappings.
-spec decode_type(atom(), type_mappings()) -> atom().
decode_type(PBType, Mods) ->
    AtomType = decode_type(PBType),
    proplists:get_value(AtomType, Mods, AtomType).

%% @doc Decodes a PB message type name into an atom type name.
-spec decode_type(atom()) -> all_type().
decode_type('COUNTER')  -> counter;
decode_type('SET')      -> set;
decode_type('HLL')      -> hll;
decode_type('REGISTER') -> register;
decode_type('FLAG')     -> flag;
decode_type('MAP')      -> map.

%% @doc Encodes an atom type into the PB message equivalent, using the
%% passed mappings to convert module names into shortnames.
-spec encode_type(atom(), type_mappings()) -> atom().
encode_type(TypeOrMod, Mods) ->
    case lists:keyfind(TypeOrMod, 2, Mods) of
        {AtomType, TypeOrMod} ->
            encode_type(AtomType);
        false ->
            encode_type(TypeOrMod)
    end.

%% @doc Encodes an atom type name into the PB message equivalent.
-spec encode_type(all_type()) -> atom().
encode_type(counter)  -> 'COUNTER';
encode_type(set)      -> 'SET';
encode_type(hll)      -> 'HLL';
encode_type(register) -> 'REGISTER';
encode_type(flag)     -> 'FLAG';
encode_type(map)      -> 'MAP'.

%% @doc Encodes a flag value into its PB message equivalent.
encode_flag_value(on) -> true;
encode_flag_value(off) -> false;
encode_flag_value(Other) -> Other.


%% ========================
%% FETCH REQUEST / RESPONSE
%% ========================

%% @doc Encodes a fetch request into a DtFetch message.
-spec encode_fetch_request({binary(), binary()}, binary()) -> #'DtFetchReq'{}.
encode_fetch_request(BucketAndType, Key) ->
    encode_fetch_request(BucketAndType, Key, []).

-spec encode_fetch_request({binary(), binary()}, binary(), [fetch_opt()]) -> #'DtFetchReq'{}.
encode_fetch_request({BType,Bucket}, Key, Options) ->
    encode_fetch_options(#'DtFetchReq'{bucket=Bucket,key=Key,type=BType}, Options).

%% @doc Encodes request-time fetch options onto the DtFetch message.
%% @private
-spec encode_fetch_options(#'DtFetchReq'{}, [fetch_opt()]) -> #'DtFetchReq'{}.
encode_fetch_options(Fetch, []) ->
    Fetch;
encode_fetch_options(Fetch, [{r,R}|Tail]) ->
    encode_fetch_options(Fetch#'DtFetchReq'{r=encode_quorum(R)},Tail);
encode_fetch_options(Fetch, [{pr,PR}|Tail]) ->
    encode_fetch_options(Fetch#'DtFetchReq'{pr=encode_quorum(PR)},Tail);
encode_fetch_options(Fetch, [basic_quorum|Tail]) ->
    encode_fetch_options(Fetch, [{basic_quorum, true}|Tail]);
encode_fetch_options(Fetch, [{basic_quorum, BQ}|Tail]) ->
    encode_fetch_options(Fetch#'DtFetchReq'{basic_quorum=BQ},Tail);
encode_fetch_options(Fetch, [notfound_ok|Tail]) ->
    encode_fetch_options(Fetch, [{notfound_ok, true}|Tail]);
encode_fetch_options(Fetch, [{notfound_ok, NOK}|Tail]) ->
    encode_fetch_options(Fetch#'DtFetchReq'{notfound_ok=NOK},Tail);
encode_fetch_options(Fetch, [{timeout, TO}|Tail]) ->
    encode_fetch_options(Fetch#'DtFetchReq'{timeout=TO},Tail);
encode_fetch_options(Fetch, [sloppy_quorum|Tail]) ->
    encode_fetch_options(Fetch, [{sloppy_quorum, true}|Tail]);
encode_fetch_options(Fetch, [{sloppy_quorum, RB}|Tail]) ->
    encode_fetch_options(Fetch#'DtFetchReq'{sloppy_quorum=RB},Tail);
encode_fetch_options(Fetch, [{n_val, N}|Tail]) ->
    encode_fetch_options(Fetch#'DtFetchReq'{n_val=N}, Tail);
encode_fetch_options(Fetch, [include_context|Tail]) ->
    encode_fetch_options(Fetch, [{include_context, true}|Tail]);
encode_fetch_options(Fetch, [{include_context, IC}|Tail]) ->
    encode_fetch_options(Fetch#'DtFetchReq'{include_context=IC},Tail);
encode_fetch_options(Fetch, [_|Tail]) ->
    encode_fetch_options(Fetch, Tail).

%% @doc Decodes a FetchResponse into tuple of type, value and context.
-spec decode_fetch_response(#'DtFetchResp'{}) -> fetch_response() | {notfound, toplevel_type()}.
decode_fetch_response(#'DtFetchResp'{type=T, value=undefined}) ->
    {notfound, decode_type(T)};
decode_fetch_response(#'DtFetchResp'{context=Context, type='COUNTER',
                                   value=#'DtValue'{counter_value=Val}}) ->
    {counter, Val, Context};
decode_fetch_response(#'DtFetchResp'{context=Context, type='SET',
                                   value=#'DtValue'{set_value=Val}}) ->
    {set, Val, Context};
decode_fetch_response(#'DtFetchResp'{context=Context, type='HLL',
                                   value=#'DtValue'{hll_value=Val}}) ->
    {hll, Val, Context};
decode_fetch_response(#'DtFetchResp'{context=Context, type='MAP',
                                   value=#'DtValue'{map_value=Val}}) ->
    {map, [ decode_map_entry(Entry) || Entry <- Val ], Context}.

%% @doc Encodes the result of a fetch request into a FetchResponse message.
-spec encode_fetch_response(toplevel_type(), toplevel_value(), context()) -> #'DtFetchResp'{}.
encode_fetch_response(Type, Value, Context) ->
    encode_fetch_response(Type, Value, Context, []).

%% @doc Encodes the result of a fetch request into a FetchResponse message.
-spec encode_fetch_response(toplevel_type(), toplevel_value(), context(),
                            type_mappings()) -> #'DtFetchResp'{}.
encode_fetch_response(Type, undefined, _Context, _Mods) ->
    #'DtFetchResp'{type=encode_type(Type)};
encode_fetch_response(Type, Value, Context, Mods) ->
    Response = #'DtFetchResp'{context=Context, type=encode_type(Type)},
    case Type of
        counter ->
            Response#'DtFetchResp'{value=#'DtValue'{counter_value=Value}};
        set ->
            Response#'DtFetchResp'{value=#'DtValue'{set_value=Value}};
        hll ->
            Response#'DtFetchResp'{value=#'DtValue'{hll_value=Value}};
        map ->
            Response#'DtFetchResp'{value=#'DtValue'{map_value=[encode_map_entry(Entry, Mods) || Entry <- Value]}}
    end.

%% =========================
%% UPDATE REQUEST / RESPONSE
%% =========================

%% @doc Decodes a CounterOp message into a counter operation.
-spec decode_counter_op(#'CounterOp'{}) -> counter_op().
decode_counter_op(#'CounterOp'{increment=Int}) when is_integer(Int) ->
    {increment, Int};
decode_counter_op(#'CounterOp'{increment=undefined}) ->
    increment.

%% @doc Encodes a counter operation into a CounterOp message.
-spec encode_counter_op(counter_op()) -> #'CounterOp'{}.
encode_counter_op({increment, Int}) when is_integer(Int) ->
    #'CounterOp'{increment=Int};
encode_counter_op(increment) ->
    #'CounterOp'{};
encode_counter_op(decrement) ->
    #'CounterOp'{increment=-1};
encode_counter_op({decrement, Int}) when is_integer(Int) ->
    #'CounterOp'{increment=(-Int)}.

%% @doc Decodes a SetOp message into a set operation.
-spec decode_set_op(#'SetOp'{}) -> set_op().
decode_set_op(#'SetOp'{adds=A, removes=[]}) ->
    {add_all, A};
decode_set_op(#'SetOp'{adds=[], removes=R}) ->
    {remove_all, R};
decode_set_op(#'SetOp'{adds=A, removes=R}) ->
    {update, [{add_all, A}, {remove_all, R}]}.

%% @doc Encodes a set operation into a SetOp message.
-spec encode_set_op(set_op()) -> #'SetOp'{}.
encode_set_op({update, Ops}) when is_list(Ops) ->
    lists:foldr(fun encode_set_update/2, #'SetOp'{}, Ops);
encode_set_op({C, _}=Op) when add == C; add_all == C;
                              remove == C; remove_all == C->
    encode_set_op({update, [Op]}).

%% @doc Folds a set update into the SetOp message.
-spec encode_set_update(simple_set_op(), #'SetOp'{}) -> #'SetOp'{}.
encode_set_update({add, Member}, #'SetOp'{adds=A}=S) when is_binary(Member) ->
    S#'SetOp'{adds=[Member|A]};
encode_set_update({add_all, Members}, #'SetOp'{adds=A}=S) when is_list(Members) ->
    S#'SetOp'{adds=Members++A};
encode_set_update({remove, Member}, #'SetOp'{removes=R}=S) when is_binary(Member) ->
    S#'SetOp'{removes=[Member|R]};
encode_set_update({remove_all, Members}, #'SetOp'{removes=R}=S) when is_list(Members) ->
    S#'SetOp'{removes=Members++R}.


%% @doc Decodes a operation name from a PB message into an atom.
-spec decode_flag_op(atom()) -> atom().

decode_flag_op('ENABLE')  -> enable;
decode_flag_op('DISABLE') -> disable.

%% @doc Encodes an atom operation name into the PB message equivalent.
-spec encode_flag_op(atom()) -> atom().
encode_flag_op(enable)  -> 'ENABLE';
encode_flag_op(disable) -> 'DISABLE'.

%% @doc Decodes a HllOp message into a hll operation.
-spec decode_hll_op(#'HllOp'{}) -> hll_op().
decode_hll_op(#'HllOp'{adds=A}) ->
    {add_all, A}.

%% @doc Encodes an hll(set) update into the HllOp message.
-spec encode_hll_op(hll_op()) -> #'HllOp'{}.
encode_hll_op({add, Member}) when is_binary(Member) ->
    #'HllOp'{adds=[Member]};
encode_hll_op({add_all, Members}) when is_list(Members) ->
    #'HllOp'{adds=Members}.

%% @doc Decodes a MapUpdate message into a map field operation.
-spec decode_map_update(#'MapUpdate'{}, type_mappings()) -> {map_field(), embedded_type_op()}.
decode_map_update(#'MapUpdate'{field=#'MapField'{name=N, type='COUNTER'=Type}, counter_op=#'CounterOp'{}=Op}, Mods) ->
    COp = decode_counter_op(Op),
    FType = decode_type(Type, Mods),
    {{N, FType}, COp};
decode_map_update(#'MapUpdate'{field=#'MapField'{name=N, type='SET'=Type}, set_op=#'SetOp'{}=Op}, Mods) ->
    SOp = decode_set_op(Op),
    FType = decode_type(Type, Mods),
    {{N, FType}, SOp};
decode_map_update(#'MapUpdate'{field=#'MapField'{name=N, type='REGISTER'=Type}, register_op=Op}, Mods) ->
    FType = decode_type(Type, Mods),
    {{N, FType}, {assign, Op}};
decode_map_update(#'MapUpdate'{field=#'MapField'{name=N, type='FLAG'=Type}, flag_op=Op}, Mods) ->
    FOp = decode_flag_op(Op),
    FType = decode_type(Type, Mods),
    {{N, FType}, FOp};
decode_map_update(#'MapUpdate'{field=#'MapField'{name=N, type='MAP'=Type}, map_op=Op}, Mods) ->
    MOp = decode_map_op(Op, Mods),
    FType = decode_type(Type, Mods),
    {{N, FType}, MOp}.

%% @doc Encodes a map field operation into a MapUpdate message.
-spec encode_map_update(map_field(), embedded_type_op()) -> #'MapUpdate'{}.
encode_map_update({_Name, counter}=Key, Op) ->
    #'MapUpdate'{field=encode_map_field(Key), counter_op=encode_counter_op(Op)};
encode_map_update({_Name, set}=Key, Op) ->
    #'MapUpdate'{field=encode_map_field(Key), set_op=encode_set_op(Op)};
encode_map_update({_Name, register}=Key, {assign, Value}) ->
    #'MapUpdate'{field=encode_map_field(Key), register_op=Value};
encode_map_update({_Name, flag}=Key, Op) ->
    #'MapUpdate'{field=encode_map_field(Key), flag_op=encode_flag_op(Op)};
encode_map_update({_Name, map}=Key, Op) ->
    #'MapUpdate'{field=encode_map_field(Key), map_op=encode_map_op(Op)}.

%% @doc Encodes a map operation into a MapOp message.
-spec encode_map_op(map_op()) -> #'MapOp'{}.
encode_map_op({update, Ops}) ->
    lists:foldr(fun encode_map_op_update/2, #'MapOp'{}, Ops);
encode_map_op({Op, _}=C) when add == Op; remove == Op ->
    encode_map_op({update, [C]});
encode_map_op({update, _Field, _Ops}=C) ->
    encode_map_op({update, [C]}).

%% @doc Folds a map update into the MapOp message.
-spec encode_map_op_update(simple_map_op(), #'MapOp'{}) -> #'MapOp'{}.
encode_map_op_update({remove, F}, #'MapOp'{removes=R}=M) ->
    M#'MapOp'{removes=[encode_map_field(F)|R]};
encode_map_op_update({update, F, Ops}, #'MapOp'{updates=U}=M) when is_list(Ops) ->
    Updates = [ encode_map_update(F, Op) || Op <- Ops ],
    M#'MapOp'{updates=Updates ++ U};
encode_map_op_update({update, F, Op}, #'MapOp'{updates=U}=M)  ->
    M#'MapOp'{updates=[encode_map_update(F, Op) | U]}.


-spec decode_map_op(#'MapOp'{}, type_mappings()) -> map_op().
decode_map_op(#'MapOp'{removes=Removes, updates=Updates}, Mods) ->
    {update,
     [ {remove, decode_map_field(R, Mods)} || R <- Removes ] ++
     [ begin
           {Field, Op} = decode_map_update(U, Mods),
           {update, Field, Op}
       end || U <- Updates ]}.

%% @doc Decodes a DtOperation message into a datatype-specific operation.
-spec decode_operation(#'DtOp'{}) -> toplevel_op().
decode_operation(Op) ->
    decode_operation(Op, []).

-spec decode_operation(#'DtOp'{}, type_mappings()) -> toplevel_op().
decode_operation(#'DtOp'{counter_op=#'CounterOp'{}=Op}, _) ->
    decode_counter_op(Op);
decode_operation(#'DtOp'{set_op=#'SetOp'{}=Op}, _) ->
    decode_set_op(Op);
decode_operation(#'DtOp'{hll_op=#'HllOp'{}=Op}, _) ->
    decode_hll_op(Op);
decode_operation(#'DtOp'{map_op=#'MapOp'{}=Op}, Mods) ->
    decode_map_op(Op, Mods).

%% @doc Encodes a datatype-specific operation into a DtOperation message.
-spec encode_operation(toplevel_op(), toplevel_type()) -> #'DtOp'{}.
encode_operation(Op, counter) ->
    #'DtOp'{counter_op=encode_counter_op(Op)};
encode_operation(Op, set) ->
    #'DtOp'{set_op=encode_set_op(Op)};
encode_operation(Op, hll) ->
    #'DtOp'{hll_op=encode_hll_op(Op)};
encode_operation(Op, map) ->
    #'DtOp'{map_op=encode_map_op(Op)}.

%% @doc Returns the type that the DtOp message expects to be performed
%% on.
-spec operation_type(#'DtOp'{}) -> toplevel_type().
operation_type(#'DtOp'{counter_op=#'CounterOp'{}}) ->
    counter;
operation_type(#'DtOp'{set_op=#'SetOp'{}}) ->
    set;
operation_type(#'DtOp'{hll_op=#'HllOp'{}}) ->
    hll;
operation_type(#'DtOp'{map_op=#'MapOp'{}}) ->
    map.

%% @doc Encodes an update request into a DtUpdate message.
-spec encode_update_request({binary(), binary()}, binary() | undefined, update()) -> #'DtUpdateReq'{}.
encode_update_request({_,_}=BucketAndType, Key, {_,_,_}=Update) ->
    encode_update_request(BucketAndType, Key, Update, []).

-spec encode_update_request({binary(), binary()}, binary() | undefined, update(), [update_opt()]) -> #'DtUpdateReq'{}.
encode_update_request({BType, Bucket}, Key, {DType, Op, Context}, Options) ->
    Update = #'DtUpdateReq'{bucket=Bucket,
                          key=Key,
                          type=BType,
                          context=Context,
                          op=encode_operation(Op, DType)},
    encode_update_options(Update, Options).

%% @doc Encodes request-time update options onto the DtUpdate message.
%% @private
-spec encode_update_options(#'DtUpdateReq'{}, [proplists:property()]) -> #'DtUpdateReq'{}.
encode_update_options(Update, []) ->
    Update;
encode_update_options(Update, [{w,W}|Tail]) ->
    encode_update_options(Update#'DtUpdateReq'{w=encode_quorum(W)},Tail);
encode_update_options(Update, [{dw,DW}|Tail]) ->
    encode_update_options(Update#'DtUpdateReq'{dw=encode_quorum(DW)},Tail);
encode_update_options(Update, [{pw,PW}|Tail]) ->
    encode_update_options(Update#'DtUpdateReq'{pw=encode_quorum(PW)},Tail);
encode_update_options(Update, [return_body|Tail]) ->
    encode_update_options(Update, [{return_body, true}|Tail]);
encode_update_options(Update, [{return_body, RB}|Tail]) ->
    encode_update_options(Update#'DtUpdateReq'{return_body=RB},Tail);
encode_update_options(Update, [{timeout, TO}|Tail]) ->
    encode_update_options(Update#'DtUpdateReq'{timeout=TO},Tail);
encode_update_options(Update, [sloppy_quorum|Tail]) ->
    encode_update_options(Update, [{sloppy_quorum, true}|Tail]);
encode_update_options(Update, [{sloppy_quorum, RB}|Tail]) ->
    encode_update_options(Update#'DtUpdateReq'{sloppy_quorum=RB},Tail);
encode_update_options(Update, [{n_val, N}|Tail]) ->
    encode_update_options(Update#'DtUpdateReq'{n_val=N}, Tail);
encode_update_options(Update, [include_context|Tail]) ->
    encode_update_options(Update, [{include_context, true}|Tail]);
encode_update_options(Update, [{include_context, IC}|Tail]) ->
    encode_update_options(Update#'DtUpdateReq'{include_context=IC},Tail);
encode_update_options(Update, [_|Tail]) ->
    encode_update_options(Update, Tail).

%% @doc Decodes a DtUpdateResp message into erlang values.
-spec decode_update_response(#'DtUpdateResp'{}, Type::toplevel_type(), ReturnBodyExpected::boolean()) ->
                                    ok | {ok, Key::binary()} | {Key::binary(), fetch_response()} | fetch_response().
decode_update_response(#'DtUpdateResp'{key=K}, _, false) ->
    case K of
        undefined -> ok;
        _ -> {ok, K}
    end;
decode_update_response(#'DtUpdateResp'{counter_value=C, context=Ctx}=Resp, counter, true) ->
    maybe_wrap_key({counter, C, Ctx}, Resp);
decode_update_response(#'DtUpdateResp'{hll_value=Hll, context=Ctx}=Resp, hll,
                       true) ->
    maybe_wrap_key({hll, Hll, Ctx}, Resp);
decode_update_response(#'DtUpdateResp'{set_value=S, context=Ctx}=Resp, set, true) ->
    maybe_wrap_key({set, S, Ctx}, Resp);
decode_update_response(#'DtUpdateResp'{map_value=M, context=Ctx}=Resp, map, true) ->
    maybe_wrap_key({map, [ decode_map_entry(F) || F <- M ], Ctx}, Resp).

maybe_wrap_key(Term, #'DtUpdateResp'{key=undefined}) -> Term;
maybe_wrap_key(Term, #'DtUpdateResp'{key=K}) -> {K, Term}.

%% @doc Encodes an update response into a DtUpdateResp message.
-spec encode_update_response(toplevel_type(), toplevel_value(), binary(),
                             context()) -> #'DtUpdateResp'{}.
encode_update_response(Type, Value, Key, Context) ->
    encode_update_response(Type, Value, Key, Context, []).

%% @doc Encodes an update response into a DtUpdateResp message.
-spec encode_update_response(toplevel_type(), toplevel_value(), binary(),
                             context(), type_mappings()) -> #'DtUpdateResp'{}.
encode_update_response(counter, Value, Key, Context, _Mods) ->
    #'DtUpdateResp'{key=Key, context=Context, counter_value=Value};
encode_update_response(set, Value, Key, Context, _Mods) ->
    #'DtUpdateResp'{key=Key, context=Context, set_value=Value};
encode_update_response(hll, Value, Key, Context, _Mods) ->
    #'DtUpdateResp'{key=Key, context=Context, hll_value=Value};
encode_update_response(map, Value, Key, Context, Mods) when is_list(Value) ->
    #'DtUpdateResp'{key=Key, context=Context,
                  map_value=[encode_map_entry(Entry, Mods)
                             || Value /= undefined,  Entry <- Value]}.
