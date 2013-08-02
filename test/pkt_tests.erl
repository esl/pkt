-module(pkt_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pkt.hrl").

%% Tests generators ------------------------------------------------------------

encapsulate_decapsulate_test_() ->
    {setup,
     fun setup/0,
     fun(_) -> ok end,
     [
      {"Test if the pacekt model is encapsulated.",
       fun encapsulate/0},
      {"Test if the packet is decapsulated to the model",
       fun decapsulate/0},
      {"Test if the initial packet matches after encapsulation and
       decapsulation",
       fun encapsulate_decapsulate/0}
     ]
    }.

%% Tests -----------------------------------------------------------------------

encapsulate() ->
    [?assert(is_binary(pkt:encapsulate(generate_packet_model())))
     || _ <- lists:seq(1, 100)].

decapsulate() ->
    [begin
         BinaryPacket = pkt:encapsulate(generate_packet_model()),
         ?assert(is_list(pkt:decapsulate(BinaryPacket)))
     end || _ <- lists:seq(1, 100)].

encapsulate_decapsulate() ->
    [begin
         PacketModel = generate_packet_model(),
         Decapsulated = pkt:decapsulate(pkt:encapsulate(PacketModel)),
         ?assertEqual(PacketModel, restore_computed_fields(Decapsulated))
     end || _ <- lists:seq(1, 100)].

%% Fixtures -------------------------------------------------------------------

setup() ->
    random:seed(erlang:now()).

%% Helper functions ------------------------------------------------------------

generate_packet_model() ->
    add_layer_header(transport, generate_payload()).

%% @private Add a TCP/IP model transport layer header to the payload.
add_layer_header(transport, Payload) ->
    Headers = [tcp, udp, none, none],
    case lists:nth(random:uniform(length(Headers)), Headers) of
        tcp ->
            add_layer_header(internet, ?IPPROTO_TCP, [#tcp{} , Payload]);
        udp ->
            add_layer_header(internet, ?IPPROTO_UDP, [#udp{} , Payload]);
        none ->
            add_layer_header(internet, none, <<>>)
    end.

%% @ private Add a header for the specified TCP/IP model layer.
add_layer_header(internet, none, _Payload) ->
    Headers = [icmp, icmpv6, ipv6_no_next],
    case lists:nth(random:uniform(length(Headers)), Headers) of
        icmp ->
            add_layer_header(link, ?ETH_P_IP, [#ipv4{p = ?IPPROTO_ICMP},
                                               #icmp{},
                                               generate_payload()]);
        icmpv6 ->
            add_layer_header(link, ?ETH_P_IPV6, [#ipv6{next = ?IPPROTO_ICMPV6,
                                                       hop = 64,
                                                       saddr = <<0:112, 1:16>>,
                                                       daddr = <<0:112, 1:16>>},
                                                 #icmpv6{},
                                                 {unsupported,
                                                  generate_payload()}]);
        ipv6_no_next ->
            add_layer_header(link, ?ETH_P_IPV6,
                             [#ipv6{next = ?IPV6_HDR_NO_NEXT_HEADER,
                                    hop = 64,
                                    saddr = <<0:112, 1:16>>,
                                    daddr = <<0:112, 1:16>>},
                              generate_payload()])
    end;
add_layer_header(internet, Proto, Payload) ->
    Headers = [ipv4, ipv6],
    case lists:nth(random:uniform(length(Headers)), Headers) of
        ipv4 ->
            add_layer_header(link, ?ETH_P_IP, [#ipv4{p = Proto} | Payload]);
        ipv6 ->
            add_layer_header(link, ?ETH_P_IPV6, [#ipv6{next = Proto,
                                                       hop = 64,
                                                       saddr = <<0:112, 1:16>>,
                                                       daddr = <<0:112, 1:16>>}
                                                 | Payload])
    end;
add_layer_header(link, EtherType, Payload) ->
    %% TODO: Add support for 802.1ad (http://en.wikipedia.org/wiki/802.1ad)
    Headers = [ieee802_1q, ether],
    case lists:nth(random:uniform(length(Headers)), Headers) of
        ieee802_1q -> [#ether{type = ?ETH_P_802_1Q},
                       #ieee802_1q_tag{vid = <<(random:uniform(16#FFF)-1):12>>,
                                       ether_type = EtherType}
                       | Payload];

        ether ->
            [#ether{type = EtherType} | Payload]
    end.

%% @private Restores fields that are computed during packet encapsulation to
%% their default values.
restore_computed_fields(Packet) ->
    lists:reverse(restore_computed_fields(Packet, [])).

restore_computed_fields([], Packet) ->
    Packet;
restore_computed_fields([#tcp{} = TcpHeder | Rest], Packet) ->
    restore_computed_fields(Rest, [TcpHeder#tcp{sum = 0} | Packet]);
restore_computed_fields([#udp{} = UdpHeder | Rest], Packet) ->
    restore_computed_fields(Rest, [UdpHeder#udp{sum = 0, ulen = 8} | Packet]);
restore_computed_fields([#icmp{} = IcmpHeader | Rest], Packet) ->
    restore_computed_fields(Rest, [IcmpHeader#icmp{checksum = 0} | Packet]);
restore_computed_fields([#icmpv6{} = Icmpv6Header | Rest], Packet) ->
    restore_computed_fields(Rest, [Icmpv6Header#icmpv6{checksum = 0} | Packet]);
restore_computed_fields([#ipv4{} = IPv4Header | Rest], Packet) ->
    restore_computed_fields(Rest, [IPv4Header#ipv4{len = 20, sum = 0} | Packet]);
restore_computed_fields([#ipv6{} = IPv6Header | Rest], Packet) ->
    restore_computed_fields(Rest, [IPv6Header#ipv6{len = 40} | Packet]);
restore_computed_fields([Header | Rest], Packet) ->
    restore_computed_fields(Rest, [Header | Packet]).

%% @priavet Generate random binary payload.
generate_payload() ->
    generate_payload(random:uniform(1000) - 1).

%% @private Generate random binary payload of the given max lenght in bytes.
generate_payload(0) ->
  <<>>;
generate_payload(MexLength) ->
    << <<(random:uniform(255))>>
       || _ <- lists:seq(1, random:uniform(MexLength) div 8) >>.
