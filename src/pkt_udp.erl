-module(pkt_udp).

-export([encapsulate/3, decapsulate/1]).

-include("pkt.hrl").

%%% API -------------------------------------------------------------------------

encapsulate(UDP, IP, Payload) ->
    UDPWithLenght = fill_length(UDP, Payload),
    construct_binary(UDPWithLenght, IP, Payload).

decapsulate(<<SrcPort:16, DstPort:16, DatagramLength:16, Checksum:16,
              Payload/binary>>) ->
    {#udp{sport = SrcPort, dport = DstPort, ulen = DatagramLength,
          sum = Checksum},
     Payload}.

%%% Helper functions ------------------------------------------------------------

fill_length(Header, Payload) ->
    Header#udp{ulen = ?UDPHDRLEN + byte_size(Payload)}.

construct_binary(UDP, IP, Payload) ->
    PseudoHeader = construct_udp_pseudo_header_binary(IP, UDP),
    IncompleteUDPHeader = construct_udp_header_binary_until_checksum(UDP),
    Checksum = pkt_checksum:compute_internet_checksum(
                 <<PseudoHeader/binary,
                   IncompleteUDPHeader/binary,
                   0:16,
                   Payload/binary>>),
    <<IncompleteUDPHeader/binary, Checksum:16, Payload/binary>>.

construct_udp_pseudo_header_binary(#ipv4{saddr = SrcAddr, daddr = DstAddr},
                                   #udp{ulen = DatagramLength}) ->
    <<SrcAddr/binary, DstAddr/binary, 0, ?IPPROTO_UDP, DatagramLength:16>>;
construct_udp_pseudo_header_binary(#ipv6{saddr = SrcAddr, daddr = DstAddr},
                                   #udp{ulen = DatagramLength}) ->
    <<SrcAddr/binary, DstAddr/binary, DatagramLength:32, 0:24, ?IPPROTO_UDP>>.

construct_udp_header_binary_until_checksum(#udp{sport = SrcPort,
                                                dport = DstPort,
                                                ulen = DatagramLength}) ->
    <<SrcPort:16, DstPort:16, DatagramLength:16>>.
