-module(pkt_checksum_tests).

-include("pkt.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_REPETITIONS, 250).

%% Test generators --------------------------------------------------------------

computation_test_() ->
    [
     {timeout, 60, {"Test if computation is correct for UDP with IPv4",
                    fun test_computation_in_udp_with_ipv4/0}},
     {timeout, 60, {"Test if computation is correct for UDP with IPv6",
                    fun test_computation_in_udp_with_ipv6/0}}
    ].

%% Tests ------------------------------------------------------------------------

test_computation_in_udp_with_ipv4() ->
    [begin
          Packet = create_random_packet_binary(udp_ipv4),
          Checksum = compute_udp_checksum(Packet),
          {timeout, 60, ?_assert(is_pdu_checksum_valid(Checksum))}
      end || _ <- lists:seq(1, ?TEST_REPETITIONS)].

test_computation_in_udp_with_ipv6() ->
    [begin
         Packet = create_random_packet_binary(udp_ipv6),
         Checksum = compute_udp_checksum(Packet),
         ?assert(is_pdu_checksum_valid(Checksum))
     end || _ <- lists:seq(1, ?TEST_REPETITIONS)].

%% Helper functions -------------------------------------------------------------

create_random_packet_binary(Headers) ->
    PacketModel = pkt_test_utils:generate_partial_packet_model(Headers),
    pkt:encapsulate(PacketModel).

compute_udp_checksum(Packet) ->
    UDPWithPseudoHdr = construct_udp_data_with_pseudo_header(Packet),
    PaddedUDPWithPseudoHdr = pkt_utils:add_padding_if_odd_length(
                               <<UDPWithPseudoHdr/binary>>),
    pkt_checksum:compute_internet_checksum(PaddedUDPWithPseudoHdr).

construct_udp_data_with_pseudo_header(_IPv4Pkt = <<4:4,
                                        HeaderLengthIn32BitWords:4,
                                        Rest/binary>>) ->
    OptionsLengthIn32BitWords = HeaderLengthIn32BitWords - 5,
    <<_:8/bytes,
      Protcol:1/binary-bytes,
      _:2/bytes,
      SrcAddr:4/binary-bytes,
      DstAddr:4/binary-bytes,
      _:OptionsLengthIn32BitWords/unit:32,
      UDPDatagram/binary>> = Rest,
    UDPLength = extract_udp_length(UDPDatagram),
    <<SrcAddr/binary, DstAddr/binary, 0, Protcol/binary, UDPLength:16,
      UDPDatagram/binary>>;
construct_udp_data_with_pseudo_header(<<6:4, _:4,
                              _:5/bytes,
                              NextHeader,
                              _,
                              SrcAddr:16/binary-bytes,
                              DstAddr:16/binary-bytes,
                              UDPDatagram/binary>>) ->
    case NextHeader of
        ?IPPROTO_UDP ->
            UDPLength = extract_udp_length(UDPDatagram),
            <<SrcAddr/binary, DstAddr/binary, UDPLength:32, 0:24, NextHeader,
              UDPDatagram/binary>>;
        _ ->
            ?assert(ipv6_extension_headers_not_supported)
    end.

is_pdu_checksum_valid(0) -> true;
is_pdu_checksum_valid(_) -> false.

extract_udp_length(<<_:4/bytes, UDPLength:16, _/binary>>) ->
    UDPLength.
