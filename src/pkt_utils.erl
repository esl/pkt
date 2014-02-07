-module(pkt_utils).

-export([add_padding_if_odd_length/1,
         compute_ones_complement/1,
         fold_into_16bit_by_adding_carry/1]).

-define(TWO_BYTES_MAX_VALUE, 16#FFFF).

%% API --------------------------------------------------------------------------

add_padding_if_odd_length(Bin) when byte_size(Bin) rem 2 =:= 0 ->
    Bin;
add_padding_if_odd_length(Bin) ->
    <<Bin/binary, 0>>.

compute_ones_complement(Bin) ->
    ?TWO_BYTES_MAX_VALUE - Bin.

fold_into_16bit_by_adding_carry(Bin) ->
    fold_into_16bit_by_adding_carry(Bin, 0).

%% Helper functions -------------------------------------------------------------

fold_into_16bit_by_adding_carry(N, Acc) when N > 0->
    NextAcc = (?TWO_BYTES_MAX_VALUE band N) + Acc,
    fold_into_16bit_by_adding_carry(N bsr 16, NextAcc);
fold_into_16bit_by_adding_carry(0, Acc) when Acc > ?TWO_BYTES_MAX_VALUE ->
    fold_into_16bit_by_adding_carry(Acc, 0);
fold_into_16bit_by_adding_carry(0, Acc) ->
    Acc.
