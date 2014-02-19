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
         Checksum = pkt_checksum_test_utils:compute_transport_layer_checksum(
                      IP, UDPBin),
         ?assert(pkt_checksum_test_utils:is_internet_checksum_valid(Checksum))

     end || _ <- lists:seq(1, ?TEST_REPETITIONS)].
