-module(pkt_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_REPETITIONS, 100).

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
    [begin
         PacketModel = pkt_test_utils:generate_packet_model(),
         ?assert(is_binary(pkt:encapsulate(PacketModel)))
     end || _ <- lists:seq(1, ?TEST_REPETITIONS)].

decapsulate() ->
    [begin
         PacketModel = pkt_test_utils:generate_packet_model(),
         BinaryPacket = pkt:encapsulate(PacketModel),
         ?assert(is_list(pkt:decapsulate(BinaryPacket)))
     end || _ <- lists:seq(1, ?TEST_REPETITIONS)].

encapsulate_decapsulate() ->
    [begin
         PacketModel = pkt_test_utils:generate_packet_model(),
         Decapsulated = pkt:decapsulate(pkt:encapsulate(PacketModel)),
         ?assertEqual(PacketModel, pkt_test_utils:restore_computed_fields(
                                     Decapsulated))
     end || _ <- lists:seq(1, ?TEST_REPETITIONS)].



%% Fixtures -------------------------------------------------------------------

setup() ->
    random:seed(erlang:now()).
