-module(pkt_udp_tests).

-include("pkt.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_REPETITIONS, 250).

%% Test generators --------------------------------------------------------------

checksum_computation_test_() ->
    [
     {timeout, 60, {"Test if computation is correct for UDP with IPv4",
                    fun() -> test_checksum(udp_ipv4) end }},
     {timeout, 60, {"Test if computation is correct for UDP with IPv6",
                    fun() -> test_checksum(udp_ipv6) end }}
    ].

%% Tests ------------------------------------------------------------------------

test_checksum(Headers) ->
    [begin
         %% GIVEN
         [IP, UDP, Payload] = pkt_test_utils:generate_partial_packet_model(
                                Headers),

         %% WHEN
         UDPBin = pkt_udp:encapsulate(UDP, IP, Payload),

         %% THEN
         Checksum = compute_udp_checksum(IP, UDPBin),
         ?assert(is_pdu_checksum_valid(Checksum))

     end || _ <- lists:seq(1, ?TEST_REPETITIONS)].

%% Helper functions -------------------------------------------------------------

compute_udp_checksum(IP, UDPBin) ->
    UDPWithPseudoHdr = construct_udp_data_with_pseudo_header(IP, UDPBin),
    PaddedUDPWithPseudoHdr = pkt_utils:add_padding_if_odd_length(
                               <<UDPWithPseudoHdr/binary>>),
    pkt_checksum:compute_internet_checksum(PaddedUDPWithPseudoHdr).

construct_udp_data_with_pseudo_header(#ipv4{saddr = SrcAddr, daddr = DstAddr},
                                      UDPBin) ->
    UDPLength = extract_udp_length(UDPBin),
    <<SrcAddr/binary, DstAddr/binary, 0, ?IPPROTO_UDP, UDPLength:16,
      UDPBin/binary>>;
construct_udp_data_with_pseudo_header(#ipv6{saddr = SrcAddr, daddr = DstAddr},
                                      UDPBin) ->
    UDPLength = extract_udp_length(UDPBin),
    <<SrcAddr/binary, DstAddr/binary, UDPLength:32, 0:24, ?IPPROTO_UDP,
      UDPBin/binary>>.

is_pdu_checksum_valid(0) -> true;
is_pdu_checksum_valid(_) -> false.

extract_udp_length(<<_:4/bytes, UDPLength:16, _/binary>>) ->
    UDPLength.
