-module(pkt_checksum_speed_tests).

-export([run/0]).

%% Internal exports
-export([fastest_checksum/1,
         alternative_checksum1/1,
         alternative_checksum2/1,
         original_checksum/1]).

-include_lib("eunit/include/eunit.hrl").

%% The maximum size of an Internet Layer PDU is 65,535.
-define(TRANSPORT_LAYER_PDU_MAX_SIZE, 65535).
-define(CHECKSUM_FUNS, [fastest_checksum,
                        alternative_checksum1,
                        alternative_checksum2,
                        original_checksum]).

-define(TIME_TEST_REPETITIONS_NUMBER, 10).
-define(TIME_TEST_BINARIES_NUMBER, 100).
-define(TIME_TEST_TIMEOUT, 30).

-define(EUNIT_TEST_DATA, <<(16#4500003044224000800600008c7c19acae241e2b):160>>).
-define(EUNIT_TEST_EXPECTED_RESULT, 16#442E).

%% Test generators --------------------------------------------------------------

run() ->
    Tests = test_correctness() ++ test_execution_time(),
    eunit:test(Tests, [verbose]).

test_correctness() ->
    [{"Testing if " ++ atom_to_list(Fun) ++ " computes correctly",
      fun() ->
              ?assertEqual(?EUNIT_TEST_EXPECTED_RESULT,
                           erlang:apply(?MODULE, Fun, [?EUNIT_TEST_DATA]))
      end} || Fun <- ?CHECKSUM_FUNS].

%% @doc Tests execution time of different checksum computation implementations,
test_execution_time() ->
    Binaries = [generate_even_length_binary()
                || _ <- lists:seq(1, ?TIME_TEST_BINARIES_NUMBER)],
    Tests = execution_time_test_suite(Binaries),
    run_tests_in_order(Tests).

execution_time_test_suite(Binaries) ->
    [{"Testing " ++ atom_to_list(Fun) ++ " speed",
      begin
          TestFun = fun() ->
                            Avg = test_execution_time(Fun, Binaries),
                            io:format(user,
                                      "~n\tAverage execution time: ~p mics ",
                                      [Avg])
                    end,
          run_test_object_in_timeout(TestFun, ?TIME_TEST_TIMEOUT)
      end}
     || Fun <- ?CHECKSUM_FUNS].


%% Checksums --------------------------------------------------------------------

%% @private Fastest checksum computation implementation. It is used by
%% the system.
fastest_checksum(Binary) ->
    pkt_checksum:compute_internet_checksum(Binary).

alternative_checksum1(Binary) ->
    Sum = sum_16bit_words(Binary),
    Folded = pkt_utils:fold_into_16bit_by_adding_carry(Sum),
    pkt_utils:compute_ones_complement(Folded).

alternative_checksum2(Binary) ->
    alternative_checksum2(Binary, 0).

alternative_checksum2(<<>>, Acc) ->
    Folded = pkt_utils:fold_into_16bit_by_adding_carry(Acc),
    pkt_utils:compute_ones_complement(Folded);
alternative_checksum2(Binary, Acc) ->
    <<Word:16, NextBytes/binary>> = Binary,
    NextAcc = Acc + Word,
    alternative_checksum2(NextBytes, NextAcc).

%% @private Checksum computation that was used at the beginning.
original_checksum(Binary) when is_binary(Binary) ->
    Sum = lists:foldl(fun original_checksum/2, 0,
                      split_binary_into_16bit_words(Binary)),
    pkt_utils:compute_ones_complement(Sum);
original_checksum(N) when N =< 16#FFFF -> N;
original_checksum(N) -> (N band 16#FFFF) + (N bsr 16).

original_checksum(N, Acc) -> original_checksum(N + Acc).

%% Helper functions -------------------------------------------------------------

run_test_object_in_timeout(TestFun, Timeout) ->
    {timeout, Timeout, TestFun}.

run_tests_in_order(Tests) ->
    {inorder, Tests}.

sum_16bit_words(Bin) ->
    SplittedBin = split_binary_into_16bit_words(Bin),
    lists:sum(SplittedBin).

split_binary_into_16bit_words(Bin) ->
    [Word || <<Word:16>> <= Bin].

test_execution_time(Function, TestsData)  ->
    SummaryResults = [begin SingleTestResults = do_test_execution_time(
                                                  Function,
                                                  [NthTestData],
                                                  ?TIME_TEST_REPETITIONS_NUMBER,
                                                  []
                                                 ),
                            compute_average(SingleTestResults)
                      end || NthTestData <- TestsData],
    compute_average(SummaryResults).

compute_average(Values) ->
    round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, Values) / length(Values)).

generate_even_length_binary() ->
    %% Subtract one so that 0-length binary can be generated.
    %% Add 1 so that maximum size binary is generated.
    Bin = pkt_test_utils:generate_binary_payload(
            random:uniform(?TRANSPORT_LAYER_PDU_MAX_SIZE + 1) - 1),
    pkt_utils:add_padding_if_odd_length(Bin).

do_test_execution_time(_Fun, _Args, Repetitions, Results)
  when Repetitions == 0 ->
    Results;
do_test_execution_time(Fun, Args, Repetitions, Results) ->
    {Time , _FunResult} = timer:tc(?MODULE, Fun, Args),
    do_test_execution_time(Fun, Args, Repetitions - 1, [Time | Results]).
