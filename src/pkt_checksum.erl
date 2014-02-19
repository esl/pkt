-module(pkt_checksum).

%% API exports
-export([compute_internet_checksum/1]).

%% API --------------------------------------------------------------------------

compute_internet_checksum(Bin) ->
    PaddedBin = pkt_utils:add_padding_if_odd_length(Bin),
    do_compute_checksum(PaddedBin, 0).

%% Helpers ----------------------------------------------------------------------

do_compute_checksum(<<Word:16, NextBytes/binary>>, Acc) ->
    NextAcc = Acc + Word,
    do_compute_checksum(NextBytes, NextAcc);
do_compute_checksum(<<>>, Acc) ->
    Folded = pkt_utils:fold_into_16bit_by_adding_carry(Acc),
    pkt_utils:compute_ones_complement(Folded).
