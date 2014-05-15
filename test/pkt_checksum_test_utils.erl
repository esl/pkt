-module(pkt_checksum_test_utils).

-include("pkt.hrl").

-export([compute_transport_layer_checksum/2, is_internet_checksum_valid/1]).

%% API --------------------------------------------------------------------------

-spec compute_transport_layer_checksum(pkt:internet_header(),
                                       pkt:transport_header()) -> integer().
compute_transport_layer_checksum(IP, TransportPDU) ->
    PDU1 = construct_transport_layer_data_with_pseudo_header(IP, TransportPDU),
    PDU2 = pkt_utils:add_padding_if_odd_length(<<PDU1/binary>>),
    pkt_checksum:compute_internet_checksum(PDU2).

-spec is_internet_checksum_valid(integer()) -> boolean().
is_internet_checksum_valid(0) -> true;
is_internet_checksum_valid(_) -> false.

%% Helper functions -------------------------------------------------------------

construct_transport_layer_data_with_pseudo_header(#ipv4{saddr = SrcAddr,
                                                        daddr = DstAddr,
                                                        p = Proto},
                                                  TransportPDU) ->
    Length = compute_transport_pdu_length(Proto, TransportPDU),
    <<SrcAddr/binary, DstAddr/binary, 0, Proto, Length:16, TransportPDU/binary>>;
construct_transport_layer_data_with_pseudo_header(#ipv6{saddr = SrcAddr,
                                                        daddr = DstAddr,
                                                        next = NextHeader},
                                                  TransportPDU) ->
    Length = compute_transport_pdu_length(NextHeader, TransportPDU),
    <<SrcAddr/binary, DstAddr/binary, Length:32, 0:24, NextHeader,
      TransportPDU/binary>>.

compute_transport_pdu_length(?IPPROTO_TCP, TCP) ->
    byte_size(TCP);
compute_transport_pdu_length(?IPPROTO_UDP, <<_:32, Length:16, _/binary>>) ->
    Length.
