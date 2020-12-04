-module(intcode_tests).

-include_lib("eunit/include/eunit.hrl").

flush() ->
    flush(0).

flush(N) ->
    receive
        _ ->
            flush(N + 1)
    after 0 ->
        N
    end.

file(Name) ->
    Src = filename:dirname(?FILE),
    filename:join([Src, "../data/", Name]).

run_get_memory(List, Position) ->
    Result = intcode:run_list(List),
    intcode:get(Position, Result).

run_io(List, Input) ->
    Program0 = intcode:from_list(List),
    Program = intcode:set_input(Input, Program0),
    Result = intcode:run(Program),
    intcode:get_output(Result).

spawn_io(List, Input) ->
    Program0 = intcode:from_list(List),
    Program1 = intcode:set_input(Input, Program0),
    Program = intcode:set_exit_pid(self(), Program1),
    Pid = intcode:spawn(Program),
    {exit, Result, _} = intcode:recvn(Pid, all, 1000),
    intcode:get_output(Result).

send_recv_test() ->
    ?assertEqual(timeout, intcode:recv(self(), 10)),

    List = [3, 4, 5],
    intcode:send(self(), List),
    ?assertEqual(List, intcode:recv(self())),

    intcode:send(self(), List),
    ?assertEqual(List, intcode:recv(none)),

    intcode:send(self(), halt),
    ?assertEqual(halt, intcode:recv(self())),

    intcode:send(self(), input),
    ?assertEqual(input, intcode:recv(self())),

    intcode:send(self(), List),
    ?assertEqual({ok, List}, intcode:recvn(self(), 3)),

    [intcode:send(self(), [Elem]) || Elem <- List],
    ?assertEqual({ok, List}, intcode:recvn(self(), 3)),

    [intcode:send(self(), [Elem]) || Elem <- List],
    [?assertEqual({ok, [Elem]}, intcode:recvn(self(), 1)) || Elem <- List],

    intcode:send(self(), List),
    [?assertEqual({ok, [Elem]}, intcode:recvn(self(), 1)) || Elem <- List],

    intcode:send(self(), List),
    ?assertEqual({timeout, List}, intcode:recvn(self(), length(List) + 1, 10)),

    intcode:send(self(), List),
    intcode:send(self(), input),
    ?assertEqual({input, List}, intcode:recvn(self(), length(List) + 1)),

    intcode:send(self(), List),
    intcode:send(self(), halt),
    ?assertEqual({halt, List}, intcode:recvn(self(), length(List) + 1)),

    intcode:send(self(), List),
    ?assertEqual({timeout, List}, intcode:recvn(self(), all, 10)),

    intcode:send(self(), List),
    intcode:send(self(), input),
    ?assertEqual({input, List}, intcode:recvn(self(), all)),

    intcode:send(self(), List),
    intcode:send(self(), halt),
    ?assertEqual({halt, List}, intcode:recvn(self(), all)),

    ?assertEqual(0, flush()).

day2_test() ->
    ?assertEqual(4690667, day2:run(star1, file("day2.data"))),
    ?assertEqual(6255, day2:run(star2, file("day2.data"))),
    ?assertEqual(0, flush()).

day5_test() ->
    ?assertEqual({11933517, [0, 0, 0, 0, 0, 0, 0, 0, 0]}, day5:run(star1, file("day5.data"))),
    ?assertEqual(10428568, day5:run(star2, file("day5.data"))),
    ?assertEqual(0, flush()).

day7_test() ->
    ?assertEqual({21860, [0, 4, 2, 1, 3]}, day7:run(star1, file("day7.data"))),

    ?assertEqual({2645740, [6, 5, 7, 8, 9]}, day7:run(star2, file("day7.data"))),
    ?assertEqual(0, flush()).

day9_test() ->
    ?assertEqual(3380552333, day9:run(star1, file("day9.data"))),
    ?assertEqual(78831, day9:run(star2, file("day9.data"))),
    ?assertEqual(0, flush()).

day11_test() ->
    ?assertEqual(2064, day11:run(star1, file("day11.data"))),
    ?assertEqual("LPZKLGHR", day11:run(star2, file("day11.data"))),
    ?assertEqual(0, flush()).

day13_test() ->
    ?assertEqual(312, day13:run(star1, file("day13.data"))),
    ?assertEqual(15909, day13:run(star2, file("day13.data"))),
    ?assertEqual(0, flush()).

day2_ex_test() ->
    ?assertEqual(2, run_get_memory([1, 0, 0, 0, 99], 0)),
    ?assertEqual(6, run_get_memory([2, 3, 0, 3, 99], 3)),
    ?assertEqual(9801, run_get_memory([2, 4, 4, 5, 99, 0], 5)),
    ?assertEqual(30, run_get_memory([1, 1, 1, 4, 99, 5, 6, 0, 99], 0)),
    ?assertEqual(2, run_get_memory([1, 1, 1, 4, 99, 5, 6, 0, 99], 4)),
    ?assertEqual(0, flush()).

day5_ex_test() ->
    F = fun(P, Input) ->
           [Op] = run_io(P, [Input]),
           Op
        end,

    %% Using position mode, consider whether the input is equal to 8; output 1
    %% (if it is) or 0 (if it is not).
    P1 = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8],
    ?assertEqual([0, 0, 0, 0, 0, 0, 0, 1, 0, 0], [F(P1, Input) || Input <- lists:seq(1, 10)]),

    %% Using position mode, consider whether the input is less than 8; output 1
    %% (if it is) or 0 (if it is not).
    P2 = [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8],
    ?assertEqual([1, 1, 1, 1, 1, 1, 1, 0, 0, 0], [F(P2, Input) || Input <- lists:seq(1, 10)]),

    %% Using immediate mode, consider whether the input is equal to 8; output 1
    %% (if it is) or 0 (if it is not).
    P3 = [3, 3, 1108, -1, 8, 3, 4, 3, 99],
    ?assertEqual([0, 0, 0, 0, 0, 0, 0, 1, 0, 0], [F(P3, Input) || Input <- lists:seq(1, 10)]),

    %% Using immediate mode, consider whether the input is less than 8; output 1
    %% (if it is) or 0 (if it is not).
    P4 = [3, 3, 1107, -1, 8, 3, 4, 3, 99],
    ?assertEqual([1, 1, 1, 1, 1, 1, 1, 0, 0, 0], [F(P4, Input) || Input <- lists:seq(1, 10)]),

    %% Here are some jump tests that take an input, then output 0 if the input
    %% was zero or 1 if the input was non-zero:
    P5 = [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9],
    P6 = [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1],
    ?assertEqual([0, 1, 1], [F(P5, Input) || Input <- lists:seq(0, 2)]),
    ?assertEqual([0, 1, 1], [F(P6, Input) || Input <- lists:seq(0, 2)]),

    %% The example program below uses an input instruction to ask for a single
    %% number. The program will then output 999 if the input value is below 8,
    %% output 1000 if the input value is equal to 8, or output 1001 if the input
    %% value is greater than 8.
    P7 = [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98,
          0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4,
          20, 1105, 1, 46, 98, 99],

    ?assertEqual([999, 999, 1000, 1001, 1001], [F(P7, Input) || Input <- lists:seq(6, 10)]),

    ?assertEqual(0, flush()).

day7_ex_test() ->
    ?assertEqual({43210, [4, 3, 2, 1, 0]}, day7:run(star1, file("day7_1.data"))),
    ?assertEqual({54321, [0, 1, 2, 3, 4]}, day7:run(star1, file("day7_2.data"))),
    ?assertEqual({65210, [1, 0, 4, 3, 2]}, day7:run(star1, file("day7_3.data"))),

    ?assertEqual({139629729, [9, 8, 7, 6, 5]}, day7:run(star2, file("day7_4.data"))),
    ?assertEqual({18216, [9, 7, 8, 5, 6]}, day7:run(star2, file("day7_5.data"))),
    ?assertEqual(0, flush()).

day9_ex_test() ->
    %% takes no input and produces a copy of itself as output.
    P1 = [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99],
    ?assertEqual(P1, run_io(P1, [])),

    %% should output a 16-digit number.
    P2 = [1102, 34915192, 34915192, 7, 4, 7, 99, 0],
    ?assertEqual([1219070632396864], run_io(P2, [])),

    %% should output the large number in the middle.
    P3 = [104, 1125899906842624, 99],
    ?assertEqual([1125899906842624], run_io(P3, [])),
    ?assertEqual(0, flush()).

day9_ex_spawn_test() ->
    %% takes no input and produces a copy of itself as output.
    P1 = [109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99],
    ?assertEqual(P1, spawn_io(P1, [])),

    %% should output a 16-digit number.
    P2 = [1102, 34915192, 34915192, 7, 4, 7, 99, 0],
    ?assertEqual([1219070632396864], spawn_io(P2, [])),

    %% should output the large number in the middle.
    P3 = [104, 1125899906842624, 99],
    ?assertEqual([1125899906842624], spawn_io(P3, [])),
    ?assertEqual(0, flush()).
