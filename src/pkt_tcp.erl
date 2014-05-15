-module(pkt_tcp).

-export([encapsulate/3, decapsulate/1]).

-include("pkt.hrl").

%%% API -------------------------------------------------------------------------

-spec encapsulate(pkt:tcp(), pkt:internet_header(), TCPPayload :: binary())
                 -> TCPBinary :: binary().
encapsulate(TCP, IP, Payload) ->
    UntilChecksum = construct_tcp_binary_until_checksum(TCP),
    BehindChecksum = construct_tcp_binary_behind_checksum(TCP, Payload),
    Length = calculate_tcp_length(TCP, Payload),
    PseudoHeader = construct_tcp_pseudo_header(IP, Length),
    Checksum = pkt_checksum:compute_internet_checksum(<<PseudoHeader/binary,
                                                        UntilChecksum/binary,
                                                        0:16,
                                                        BehindChecksum/binary>>),
    <<UntilChecksum/binary, Checksum:16, BehindChecksum/binary>>.

-spec decapsulate(TCPBinary :: binary()) -> {pkt:tcp(), TCPPayload :: binary()}.
decapsulate(<<SPort:16, DPort:16,
              SeqNo:32,
              AckNo:32,
              Off:4, 0:4, CWR:1, ECE:1, URG:1, ACK:1,
              PSH:1, RST:1, SYN:1, FIN:1, Win:16,
              Sum:16, Urp:16,
              Rest/binary>>
           ) when Off >= 5 ->
    {Opt, Payload} = separate_options_from_payload(Off, Rest),
    {#tcp{sport = SPort, dport = DPort,
          seqno = SeqNo,
          ackno = AckNo,
          off = Off, cwr = CWR, ece = ECE, urg = URG, ack = ACK,
          psh = PSH, rst = RST, syn = SYN, fin = FIN, win = Win,
          sum = Sum, urp = Urp,
          opt = Opt}, Payload}.

%%% Helper functions ------------------------------------------------------------

construct_tcp_binary_until_checksum(
  #tcp{
     sport = SPort, dport = DPort,
     seqno = SeqNo,
     ackno = AckNo,
     off = Off, cwr = CWR, ece = ECE, urg = URG, ack = ACK,
     psh = PSH, rst = RST, syn = SYN, fin = FIN, win = Win}) ->
    <<SPort:16, DPort:16,
      SeqNo:32,
      AckNo:32,
      Off:4, 0:4, CWR:1, ECE:1, URG:1, ACK:1,
      PSH:1, RST:1, SYN:1, FIN:1, Win:16>>.

construct_tcp_binary_behind_checksum(#tcp{urp = Urp, opt = Opt}, Payload) ->
    <<Urp:16, Opt/binary, Payload/binary>>.

calculate_tcp_length(#tcp{opt = Opts}, Payload) ->
    ?TCPHDRLEN + byte_size(Opts) + byte_size(Payload).

construct_tcp_pseudo_header(#ipv4{saddr = SrcAddr, daddr = DstAddr},
                            DatagramLength) ->
    <<SrcAddr/binary, DstAddr/binary, 0, ?IPPROTO_TCP, DatagramLength:16>>;
construct_tcp_pseudo_header(#ipv6{saddr = SrcAddr, daddr = DstAddr},
                            DatagramLength) ->
    <<SrcAddr/binary, DstAddr/binary, DatagramLength:32, 0:24, ?IPPROTO_TCP>>.

separate_options_from_payload(Offset, OptsWithPayload) ->
    OptsBytes = (Offset - 5)*4,
    <<Opts:OptsBytes/binary, Payload/binary>> = OptsWithPayload,
    {Opts, Payload}.
