-module(pkt_tcp).

-export([encapsulate/3, decapsulate/1]).

-include("pkt.hrl").

%%% API -------------------------------------------------------------------------

decapsulate(
  <<SPort:16, DPort:16,
    SeqNo:32,
    AckNo:32,
    Off:4, 0:4, CWR:1, ECE:1, URG:1, ACK:1,
    PSH:1, RST:1, SYN:1, FIN:1, Win:16,
    Sum:16, Urp:16,
    Rest/binary>>
 ) when Off >= 5 ->
    {Opt, Payload} = options(Off, Rest),
    {#tcp{
        sport = SPort, dport = DPort,
        seqno = SeqNo,
        ackno = AckNo,
        off = Off, cwr = CWR, ece = ECE, urg = URG, ack = ACK,
        psh = PSH, rst = RST, syn = SYN, fin = FIN, win = Win,
        sum = Sum, urp = Urp,
        opt = Opt
       }, Payload}.

encapsulate(IP, TCP, Payload) ->
    HdrBin = tcp(TCP#tcp{sum = makesum([IP, TCP, Payload])}),
    <<HdrBin/binary, Payload/binary>>.

%%% Helper functions ------------------------------------------------------------

tcp(#tcp{
       sport = SPort, dport = DPort,
       seqno = SeqNo,
       ackno = AckNo,
       off = Off, cwr = CWR, ece = ECE, urg = URG, ack = ACK,
       psh = PSH, rst = RST, syn = SYN, fin = FIN, win = Win,
       sum = Sum, urp = Urp, opt = Opt
      }) ->
    <<SPort:16, DPort:16,
      SeqNo:32,
      AckNo:32,
      Off:4, 0:4, CWR:1, ECE:1, URG:1, ACK:1,
      PSH:1, RST:1, SYN:1, FIN:1, Win:16,
      Sum:16, Urp:16, Opt/binary >>.

options(Offset, Payload) ->
    N = (Offset-5)*4,
    <<Opt:N/binary, Payload1/binary>> = Payload,
    {Opt, Payload1}.

makesum(Hdr) -> 16#FFFF - checksum(Hdr).

checksum([IP, TransportLayer, Payload]) ->
    checksum(IP, TransportLayer, Payload);
checksum(Hdr) ->
    lists:foldl(fun compl/2, 0, [ W || <<W:16>> <= Hdr ]).

compl(N) when N =< 16#FFFF -> N;
compl(N) -> (N band 16#FFFF) + (N bsr 16).
compl(N,S) -> compl(N+S).

checksum(#ipv4{saddr = SAddr,
               daddr = DAddr},
         #tcp{} = TCPhdr,
         Payload) ->
    TCP = tcp(TCPhdr#tcp{sum = 0}),
    Len = size(TCP) + size(Payload),
    Pad = 8 * (Len rem 2),
    checksum(<<SAddr:32/bits,
               DAddr:32/bits,
               0:8, ?IPPROTO_TCP:8,
               Len:16,
               TCP/binary,
               Payload/bits,
               0:Pad>>);

checksum(#ipv6{saddr = SAddr,
               daddr = DAddr},
         #tcp{} = TCPhdr,
         Payload) ->
    TCP = tcp(TCPhdr#tcp{sum = 0}),
    Len = size(TCP) + size(Payload),
    Pad = 8 * (Len rem 2),
    checksum(<<SAddr:128/bits,
               DAddr:128/bits,
               Len:32,
               0:24, ?IPPROTO_TCP:8,
               TCP/binary,
               Payload/bits,
               0:Pad>>).
