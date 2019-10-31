%% -------------------------------------------------------------------
%%
%% riak_pb_kv_codec: protocol buffer utility functions for Riak KV messages
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

%% @doc Utility functions for decoding and encoding Protocol Buffers
%% messages related to Riak KV. These are used inside the client and
%% server code and do not normally need to be used in application
%% code.

-module(riak_pb_kv_codec).

-include("riak_kv_pb.hrl").
-include("riak_pb_kv_codec.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([encode_contents/1,     %% riakc_pb:pbify_rpbcontents
         decode_contents/1,     %% riakc_pb:erlify_rpbcontents
         encode_content/1,      %% riakc_pb:pbify_rpbcontent
         decode_content/1,      %% riakc_pb:erlify_rpbcontent
         encode_content_meta/3, %% riakc_pb:pbify_rpbcontent_entry
         decode_content_meta/3,
         encode_pair/1,         %% riakc_pb:pbify_rpbpair
         encode_index_pair/1,
         decode_pair/1,         %% riakc_pb:erlify_rpbpair
         encode_link/1,         %% riakc_pb:pbify_rpblink
         decode_link/1,         %% riakc_pb:erlify_rpblink
         encode_quorum/1,
         decode_quorum/1,       %% riak_kv_pb_socket:normalize_rw_value
         encode_apl_ann/1,
         encode_ring/1,
         decode_ring/1
        ]).

-export_type([quorum/0]).
-type quorum() :: symbolic_quorum() | non_neg_integer().
-type symbolic_quorum() :: one | quorum | all | default.
-type value() :: binary().

-ifdef(namespaced_types).
-type metadata() :: dict:dict(binary(), binary()).
-else.
-type metadata() :: dict().
-endif.

-type contents() :: [{metadata(), value()}].

%% @doc Annotated preflist type
-type preflist_with_pnum_ann() :: [{{non_neg_integer(), node()}, primary|fallback}].


%% @doc Convert a list of object {MetaData,Value} pairs to protocol
%% buffers messages.
-spec encode_contents(contents()) -> [#rpbcontent{}].
encode_contents(List) ->
    [ encode_content(C) || C <- List ].

%% @doc Convert a metadata/value pair into an #rpbcontent{} record
-spec encode_content({metadata(), value()}) -> #rpbcontent{}.
encode_content({MetadataIn, ValueIn}=C) ->
    {Metadata, Value} =
        case is_binary(ValueIn) of
            true ->
                C;
            false ->
                %% If the riak object was created using
                %% the native erlang interface, it is possible
                %% for the value to consist of arbitrary terms.
                %% PBC needs to send a binary, so replace the content type
                %% to mark it as an erlang binary and encode
                %% the term as a binary.
                {dict:store(?MD_CTYPE, ?CTYPE_ERLANG_BINARY, MetadataIn),
                 term_to_binary(ValueIn)}
        end,
    dict:fold(fun encode_content_meta/3, #rpbcontent{value = Value}, Metadata).

%% @doc Convert the metadata dictionary entries to protocol buffers
-spec encode_content_meta(MetadataKey::string(), any(), tuple()) -> tuple().
encode_content_meta(?MD_CTYPE, ContentType, PbContent) when is_list(ContentType) ->
    PbContent#rpbcontent{content_type = ContentType};
encode_content_meta(?MD_CHARSET, Charset, PbContent) when is_list(Charset) ->
    PbContent#rpbcontent{charset = Charset};
encode_content_meta(?MD_ENCODING, Encoding, PbContent) when is_list(Encoding) ->
    PbContent#rpbcontent{content_encoding = Encoding};
encode_content_meta(?MD_VTAG, Vtag, PbContent) when is_list(Vtag) ->
    PbContent#rpbcontent{vtag = Vtag};
encode_content_meta(?MD_LINKS, Links, PbContent) when is_list(Links) ->
    PbContent#rpbcontent{links = [encode_link(E) || E <- Links]};
encode_content_meta(?MD_LASTMOD, {MS,S,US}, PbContent) ->
    PbContent#rpbcontent{last_mod = 1000000*MS+S, last_mod_usecs = US};
encode_content_meta(?MD_USERMETA, UserMeta, PbContent) when is_list(UserMeta) ->
    PbContent#rpbcontent{usermeta = [encode_pair(E) || E <- UserMeta]};
encode_content_meta(?MD_INDEX, Indexes, PbContent) when is_list(Indexes) ->
    PbContent#rpbcontent{indexes = [encode_index_pair(E) || E <- Indexes]};
encode_content_meta(?MD_DELETED, DeletedVal, PbContent) ->
    PbContent#rpbcontent{deleted=header_val_to_bool(DeletedVal)};
encode_content_meta(_Key, _Value, PbContent) ->
    %% Ignore unknown metadata - need to add to RpbContent if it needs to make it
    %% to/from the client
    PbContent.

%% @doc Return a boolean based on a header value.
%% Representations of `true' return `true'; anything
%% else returns `false'.
-spec header_val_to_bool(term()) -> boolean().
header_val_to_bool(<<"true">>) ->
    true;
header_val_to_bool("true") ->
    true;
header_val_to_bool(true) ->
    true;
header_val_to_bool(_) ->
    false.

%% @doc Convert a list of rpbcontent pb messages to a list of [{MetaData,Value}] tuples
-spec decode_contents(PBContents::[tuple()]) -> contents().
decode_contents(RpbContents) ->
    [decode_content(RpbContent) || RpbContent <- RpbContents].

-spec decode_content_meta(atom(), any(), #rpbcontent{}) -> [ {binary(), any()} ].
decode_content_meta(_, undefined, _Pb) ->
    [];
decode_content_meta(_, [], _Pb) ->
    %% Repeated metadata fields that are empty lists need not be added
    %% to the decoded metadata. This previously resulted in
    %% type-conversion errors when using the JSON form of a
    %% riak_object. All of the other metadata types are primitive
    %% types.
    [];
decode_content_meta(content_type, CType, _Pb) ->
    [{?MD_CTYPE, binary_to_list(CType)}];
decode_content_meta(charset, Charset, _Pb) ->
    [{?MD_CHARSET, binary_to_list(Charset)}];
decode_content_meta(encoding, Encoding, _Pb) ->
    [{?MD_ENCODING, binary_to_list(Encoding)}];
decode_content_meta(vtag, VTag, _Pb) ->
    [{?MD_VTAG, binary_to_list(VTag)}];
decode_content_meta(last_mod, LastMod, Pb) ->
    case Pb#rpbcontent.last_mod_usecs of
        undefined ->
            Usec = 0;
        Usec ->
            Usec
    end,
    Msec = LastMod div 1000000,
    Sec = LastMod rem 1000000,
    [{?MD_LASTMOD, {Msec,Sec,Usec}}];
decode_content_meta(links, Links1, _Pb) ->
    Links = [ decode_link(L) || L <- Links1 ],
    [{?MD_LINKS, Links}];
decode_content_meta(usermeta, PbUserMeta, _Pb) ->
    UserMeta = [decode_pair(E) || E <- PbUserMeta],
    [{?MD_USERMETA, UserMeta}];
decode_content_meta(indexes, PbIndexes, _Pb) ->
    Indexes = [decode_pair(E) || E <- PbIndexes],
    [{?MD_INDEX, Indexes}];
decode_content_meta(deleted, DeletedVal, _Pb) ->
    [{?MD_DELETED, DeletedVal}].


%% @doc Convert an rpccontent pb message to an erlang {MetaData,Value} tuple
-spec decode_content(PBContent::tuple()) -> {metadata(), binary()}.
decode_content(PbC) ->
    MD =  decode_content_meta(content_type, PbC#rpbcontent.content_type, PbC) ++
          decode_content_meta(charset, PbC#rpbcontent.charset, PbC) ++
          decode_content_meta(encoding, PbC#rpbcontent.content_encoding, PbC) ++
          decode_content_meta(vtag, PbC#rpbcontent.vtag, PbC) ++
          decode_content_meta(links, PbC#rpbcontent.links, PbC) ++
          decode_content_meta(last_mod, PbC#rpbcontent.last_mod, PbC) ++
          decode_content_meta(usermeta, PbC#rpbcontent.usermeta, PbC) ++
          decode_content_meta(indexes, PbC#rpbcontent.indexes, PbC) ++
          decode_content_meta(deleted, PbC#rpbcontent.deleted, PbC),

    {dict:from_list(MD), PbC#rpbcontent.value}.

%% @doc Convert {K,V} index entries into protocol buffers
-spec encode_index_pair({binary(), integer() | binary()}) -> #rpbpair{}.
encode_index_pair({K,V}) when is_integer(V) ->
    encode_pair({K, integer_to_list(V)});
encode_index_pair(E) ->
    encode_pair(E).

%% @doc Convert {K,V} tuple to protocol buffers
%% @equiv riak_pb_codec:encode_pair/1
-spec encode_pair({Key::binary(), Value::any()}) -> #rpbpair{}.
encode_pair(Pair) ->
    riak_pb_codec:encode_pair(Pair).

%% @doc Convert RpbPair PB message to erlang {K,V} tuple
%% @equiv riak_pb_codec:decode_pair/1
-spec decode_pair(#rpbpair{}) -> {binary(), binary()}.
decode_pair(PB) ->
    riak_pb_codec:decode_pair(PB).

%% @doc Convert erlang link tuple to RpbLink PB message
-spec encode_link({{binary(), binary()}, binary() | string()}) -> #rpblink{}.
encode_link({{B,K},T}) ->
    #rpblink{bucket = B, key = K, tag = T}.

%% @doc Convert RpbLink PB message to erlang link tuple
-spec decode_link(PBLink::#rpblink{}) -> {{binary(), binary()}, binary()}.
decode_link(#rpblink{bucket = B, key = K, tag = T}) ->
    {{B,K},T}.

%% @doc Encode a symbolic or numeric quorum value into a Protocol
%% Buffers value
-spec encode_quorum(quorum()) -> non_neg_integer().
encode_quorum(Bin) when is_binary(Bin) -> encode_quorum(binary_to_existing_atom(Bin, latin1));
encode_quorum(one) -> ?RIAKPB_RW_ONE;
encode_quorum(quorum) -> ?RIAKPB_RW_QUORUM;
encode_quorum(all) -> ?RIAKPB_RW_ALL;
encode_quorum(default) -> ?RIAKPB_RW_DEFAULT;
encode_quorum(undefined) -> undefined;
encode_quorum(I) when is_integer(I), I >= 0 -> I.

%% @doc Decodes a Protocol Buffers value into a symbolic or numeric
%% quorum.
-spec decode_quorum(non_neg_integer()) -> quorum().
decode_quorum(?RIAKPB_RW_ONE) -> one;
decode_quorum(?RIAKPB_RW_QUORUM) -> quorum;
decode_quorum(?RIAKPB_RW_ALL) -> all;
decode_quorum(?RIAKPB_RW_DEFAULT) -> default;
decode_quorum(undefined) -> undefined;
decode_quorum(I) when is_integer(I), I >= 0 -> I.

%% @doc Convert preflist to RpbBucketKeyPreflist.
-spec encode_apl_ann(preflist_with_pnum_ann()) ->
                            PBPreflist::[#rpbbucketkeypreflistitem{}].
encode_apl_ann(Preflist) ->
    [encode_apl_item({PartitionNumber, Node}, T) ||
        {{PartitionNumber, Node}, T} <- Preflist].

-spec encode_apl_item({non_neg_integer(), node()}, primary|fallback) ->
                            #rpbbucketkeypreflistitem{}.
encode_apl_item({PartitionNumber, Node}, primary) ->
    #rpbbucketkeypreflistitem{partition=PartitionNumber,
                              node=riak_pb_codec:to_binary(Node),
                              primary=riak_pb_codec:encode_bool(true)};
encode_apl_item({PartitionNumber, Node}, fallback) ->
    #rpbbucketkeypreflistitem{partition=PartitionNumber,
                              node=riak_pb_codec:to_binary(Node),
                              primary=riak_pb_codec:encode_bool(false)}.

-spec encode_ring(Ring :: #riak_pb_ring{}) ->
    #rpbgetringresp{}.
encode_ring(Ring) when erlang:is_record(Ring, riak_pb_ring) ->
    #riak_pb_ring{nodename = NodeName, vclock = VClockList, chring = ChRing, meta = Meta, clustername = ClusterName,
        next = NextList, members = MembersList, claimant = Claimant, seen = SeenList, rvsn = Rvsn} = Ring,
    EncodedNodeName = erlang:term_to_binary(NodeName),
    EncodedVClock = encode_vclock_list(VClockList),
    EncodedChRing = encode_chring_object(ChRing),
    EncodedMeta = erlang:term_to_binary(Meta),
    EncodedClusterName = encode_cluster_name_object(ClusterName),
    EncodedNext = [encode_next_object(NextObject) || NextObject <- NextList],
    EncodedMembers = [encode_members_object(MembersObject) || MembersObject <- MembersList],
    EncodedClaimant = encode_claimant_object(Claimant),
    EncodedSeen = [encode_seen_object(SeenObject) || SeenObject <- SeenList],
    EncodedRvsn = encode_vclock_list(Rvsn),
    #rpbgetringresp{node_name = EncodedNodeName, vclock = EncodedVClock, chring = EncodedChRing, meta = EncodedMeta,
        cluster_name = EncodedClusterName, next = EncodedNext, members = EncodedMembers, claimant = EncodedClaimant,
        seen = EncodedSeen, rvsn = EncodedRvsn}.

-spec decode_ring(Ring :: #rpbgetringresp{}) ->
    #riak_pb_ring{}.
decode_ring(Ring) when erlang:is_record(Ring, rpbgetringresp) ->
    #rpbgetringresp{node_name = EncodedNodeName, vclock = EncodedVClockList, chring = EncodedChRing, meta = EncodedMeta,
        cluster_name = EncodedClusterName, next = EncodedNextList, members = EncodedMembersList,
        claimant = EncodedClaimant, seen = EncodedSeenList, rvsn = EncodedRvsnList} = Ring,
    NodeName = erlang:binary_to_term(EncodedNodeName),
    VClock = decode_vclock_list(EncodedVClockList),
    ChRing = decode_chring_object(EncodedChRing),
    Meta = erlang:binary_to_term(EncodedMeta),
    ClusterName = decode_cluster_name_object(EncodedClusterName),
    Next = [decode_next_object(EncodedNextObject) || EncodedNextObject <- EncodedNextList],
    Members = decode_members_list(EncodedMembersList),
    Claimant = decode_claimant_object(EncodedClaimant),
    Seen = decode_seen_list(EncodedSeenList),
    Rvsn = decode_vclock_list(EncodedRvsnList),
    #riak_pb_ring{nodename = NodeName, vclock = VClock, chring = ChRing, meta = Meta, clustername = ClusterName,
        next = Next, members = Members, claimant = Claimant, seen = Seen, rvsn = Rvsn}.

encode_vclock_list(undefined) ->
    undefined;
encode_vclock_list(VClockList) ->
    EncodedVClockList = [encode_vclock_object(VClockObject) || VClockObject <- VClockList],
    #vclocklist{vclock = EncodedVClockList}.

encode_vclock_object({Node, TimeStamp}) ->
    EncodedNode = erlang:term_to_binary(Node),
    EncodedTimeStamp = erlang:term_to_binary(TimeStamp),
    #vclockobject{vclock_node = EncodedNode, timestamp = EncodedTimeStamp}.

decode_vclock_list(undefined) ->
    undefined;
decode_vclock_list(VClockList) when erlang:is_record(VClockList, vclocklist) ->
    #vclocklist{vclock = VClockList} = VClockList,
    [decode_vclock_object(VClockObject) || VClockObject <- VClockList].

decode_vclock_object(VClockObject) when erlang:is_record(VClockObject, vclockobject) ->
    #vclockobject{vclock_node = EncodedVClockNode, timestamp = EncodedTimeStamp} = VClockObject,
    VClockNode = erlang:binary_to_term(EncodedVClockNode),
    Timestamp = erlang:binary_to_term(EncodedTimeStamp),
    {VClockNode, Timestamp}.

encode_chring_object({NumPartitions, NodeEntries}) ->
    EncodedNodeEntries = erlang:term_to_binary(NodeEntries),
    #chringobject{num_partitions = NumPartitions, node_entries = EncodedNodeEntries}.

decode_chring_object(ChRingObject) when erlang:is_record(ChRingObject, chringobject) ->
    #chringobject{num_partitions = NumPartitions, node_entries = EncodedNodeEntries} = ChRingObject,
    NodeEntries = erlang:binary_to_term(EncodedNodeEntries),
    {NumPartitions, NodeEntries}.

encode_cluster_name_object(undefined) ->
    undefined;
encode_cluster_name_object({Node, TimeStamp}) ->
    EncodedNode = erlang:term_to_binary(Node),
    EncodedTimeStamp = erlang:term_to_binary(TimeStamp),
    #clusternameobject{node = EncodedNode, timestamp = EncodedTimeStamp}.

decode_cluster_name_object(undefined) ->
    undefined;
decode_cluster_name_object(ClusterNameObject) when erlang:is_record(ClusterNameObject, clusternameobject) ->
    #clusternameobject{node = EncodedNode, timestamp = EncodedTimeStamp} = ClusterNameObject,
    Node = erlang:binary_to_term(EncodedNode),
    TimeStamp = erlang:binary_to_term(EncodedTimeStamp),
    {Node, TimeStamp}.

encode_next_object({Index, Owner, NextOwner, Transfers, Status}) ->
    EncodedOwner = erlang:term_to_binary(Owner),
    EncodedNextOwner = erlang:term_to_binary(NextOwner),
    EncodedTransfers = erlang:term_to_binary(Transfers),
    EncodedStatus = erlang:term_to_binary(Status),
    #nextobject{index = Index, owner = EncodedOwner, next_owner = EncodedNextOwner, transfers = EncodedTransfers,
        status = EncodedStatus}.

decode_next_object(NextObject) when erlang:is_record(NextObject, nextobject) ->
    #nextobject{index = Index, owner = EncodedOwner, next_owner = EncodedNextOwner, transfers = EncodedTransfers,
        status = EncodedStatus} = NextObject,
    Owner = erlang:binary_to_term(EncodedOwner),
    NextOwner = erlang:binary_to_term(EncodedNextOwner),
    Transfers = erlang:binary_to_term(EncodedTransfers),
    Status = erlang:binary_to_term(EncodedStatus),
    {Index, Owner, NextOwner, Transfers, Status}.

encode_members_object({Node, MembersInformation}) ->
    EncodedNode = erlang:term_to_binary(Node),
    EncodedMembersInformation = encode_members_information(MembersInformation),
    #membersobject{node = EncodedNode, members_information = EncodedMembersInformation}.

encode_members_information({Status, VClock, MembersOptionsList}) ->
    EncodedStatus = erlang:term_to_binary(Status),
    EncodedVClock = encode_vclock_list(VClock),
    EncodedMembersOptions = [encode_members_options(MembersOptionsObject) || MembersOptionsObject <- MembersOptionsList],
    #membersinformationobject{status = EncodedStatus, vclock = EncodedVClock, members_options = EncodedMembersOptions}.

encode_members_options({OptionKey, OptionValue}) ->
    EncodedOptionKey = erlang:term_to_binary(OptionKey),
    EncodedOptionValue = erlang:term_to_binary(OptionValue),
    #membersoptionsobject{option_key = EncodedOptionKey, option_value = EncodedOptionValue}.

encode_claimant_object(undefined) ->
    undefined;
encode_claimant_object(Claimant) ->
    erlang:term_to_binary(Claimant).

encode_seen_object({Node, VClock}) ->
    EncodedNode = erlang:term_to_binary(Node),
    EncodedVClock = encode_vclock_list(VClock),
    #seenobject{node = EncodedNode, vclock = EncodedVClock}.


-ifdef(TEST).

encode_apl_ann_test() ->
    Encoded = encode_apl_ann([{{1,
                                'dev5@127.0.0.1'},
                               primary},
                              {{2,
                                'dev6@127.0.0.1'},
                               primary},
                              {{3,
                                'dev3@127.0.0.1'},
                               fallback}]),
    ?assertEqual(Encoded,
                 [{rpbbucketkeypreflistitem,
                   1,<<"dev5@127.0.0.1">>,true},
                  {rpbbucketkeypreflistitem,
                   2,<<"dev6@127.0.0.1">>,true},
                  {rpbbucketkeypreflistitem,
                   3,<<"dev3@127.0.0.1">>,false}]).

-endif.
