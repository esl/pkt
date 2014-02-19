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
         Checksum = pkt_checksum_test_utils:compute_transport_layer_checksum(
                      IP, TCPBin),
         ?assert(pkt_checksum_test_utils:is_internet_checksum_valid(Checksum))

     end || _ <- lists:seq(1, ?TEST_REPETITIONS)].
