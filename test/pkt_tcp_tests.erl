-module(pkt_tcp_tests).

-include("pkt.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_REPETITIONS, 250).

%% Test generators --------------------------------------------------------------

checksum_computation_test_() ->
    [
     {timeout, 60, {"Test if computation is correct for TCP with IPv4",
                    fun() -> test_checksum(tcp_ipv4) end }},
     {timeout, 60, {"Test if computation is correct for TCP with IPv6",
                    fun() -> test_checksum(tcp_ipv6) end }}
    ].

%% Tests ------------------------------------------------------------------------

test_checksum(Headers) ->
    [begin
         %% GIVEN
         [IP, TCP, Payload] = pkt_test_utils:generate_partial_packet_model(
                                Headers),

         %% WHEN
         TCPBin = pkt_tcp:encapsulate(IP, TCP, Payload),

         %% THEN
         Checksum = compute_tcp_checksum(IP, TCPBin),
         ?assert(is_pdu_checksum_valid(Checksum))

     end || _ <- lists:seq(1, ?TEST_REPETITIONS)].

%% Helper functions -------------------------------------------------------------

compute_tcp_checksum(IP, TCPBin) ->
    TCPWithPseudoHdr = construct_tcp_data_with_pseudo_header(IP, TCPBin),
    PaddedTCPWithPseudoHdr = pkt_utils:add_padding_if_odd_length(
                               <<TCPWithPseudoHdr/binary>>),
    pkt_checksum:compute_internet_checksum(PaddedTCPWithPseudoHdr).

construct_tcp_data_with_pseudo_header(#ipv4{saddr = SrcAddr, daddr = DstAddr},
                                      TCPBin) ->
    TCPLength = compute_tcp_length(TCPBin),
    <<SrcAddr/binary, DstAddr/binary, 0, ?IPPROTO_TCP, TCPLength:16,
      TCPBin/binary>>;
construct_tcp_data_with_pseudo_header(#ipv6{saddr = SrcAddr, daddr = DstAddr},
                                      TCPBin) ->
    TCPLength = compute_tcp_length(TCPBin),
    <<SrcAddr/binary, DstAddr/binary, TCPLength:32, 0:24, ?IPPROTO_TCP,
      TCPBin/binary>>.

is_pdu_checksum_valid(0) -> true;
is_pdu_checksum_valid(_) -> false.

compute_tcp_length(TCP) ->
    byte_size(TCP).
