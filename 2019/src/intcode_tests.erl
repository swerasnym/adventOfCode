-module(intcode_tests).
-include_lib("eunit/include/eunit.hrl").

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

day2_test() ->
    4690667 = day2:run(star1, file("day2.data")),
    6255 = day2:run(star2, file("day2.data")),
    ok.

day5_test() ->
    [0,0,0,0,0,0,0,0,0,11933517] = day5:run(star1, file("day5.data")),
    [10428568] = day5:run(star2, file("day5.data")),
    ok.

day7_test() ->
    {21860,[0,4,2,1,3]} = day7:run(star1, file("day7.data")),


    {2645740, [6,5,7,8,9]} = day7:run(star2, file("day7.data")),
    ok.

day9_test() ->
    [3380552333] = day9:run(star1, file("day9.data")),
    [78831] = day9:run(star2, file("day9.data")),
    ok.


day2_ex_test() ->
    2 = run_get_memory([1,0,0,0,99], 0),
    6 = run_get_memory([2,3,0,3,99], 3),
    9801 = run_get_memory([2,4,4,5,99,0], 5),
    30 = run_get_memory([1,1,1,4,99,5,6,0,99], 0),
    2 = run_get_memory([1,1,1,4,99,5,6,0,99], 4),
    ok.

day5_ex_test() ->

    F = fun(P, Input) ->
		[Op] = run_io(P, [Input]),
		Op
	end,

    %% Using position mode, consider whether the input is equal to 8; output 1
    %% (if it is) or 0 (if it is not).
    P1 = [3,9,8,9,10,9,4,9,99,-1,8],
    [0,0,0,0,0,0,0,1,0,0] = [F(P1, Input) || Input <- lists:seq(1,10)],

    %% Using position mode, consider whether the input is less than 8; output 1
    %% (if it is) or 0 (if it is not).
    P2 = [3,9,7,9,10,9,4,9,99,-1,8],
    [1,1,1,1,1,1,1,0,0,0] = [F(P2, Input) || Input <- lists:seq(1,10)],

    %% Using immediate mode, consider whether the input is equal to 8; output 1
    %% (if it is) or 0 (if it is not).
    P3 = [3,3,1108,-1,8,3,4,3,99],
    [0,0,0,0,0,0,0,1,0,0] = [F(P3, Input) || Input <- lists:seq(1,10)],

    %% Using immediate mode, consider whether the input is less than 8; output 1
    %% (if it is) or 0 (if it is not).
    P4 = [3,3,1107,-1,8,3,4,3,99],
    [1,1,1,1,1,1,1,0,0,0] = [F(P4, Input) || Input <- lists:seq(1,10)],

    %% Here are some jump tests that take an input, then output 0 if the input
    %% was zero or 1 if the input was non-zero:
    P5 = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9],
    P6 = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1],
    [0,1,1] = [F(P5, Input) || Input <- lists:seq(0,2)],
    [0,1,1] = [F(P6, Input) || Input <- lists:seq(0,2)],

    %% The example program below uses an input instruction to ask for a single
    %% number. The program will then output 999 if the input value is below 8,
    %% output 1000 if the input value is equal to 8, or output 1001 if the input
    %% value is greater than 8.
    P7 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
	  1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
	  999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99],

    [999,999,1000,1001,1001] = [F(P7, Input) || Input <- lists:seq(6,10)],

    ok.

day7_ex_test() ->
    {43210, [4,3,2,1,0]} = day7:run(star1, file("day7_1.data")),
    {54321, [0,1,2,3,4]} = day7:run(star1, file("day7_2.data")),
    {65210, [1,0,4,3,2]} = day7:run(star1, file("day7_3.data")),

    {139629729, [9,8,7,6,5]} = day7:run(star2, file("day7_4.data")),
    {18216, [9,7,8,5,6]} = day7:run(star2, file("day7_5.data")),
    ok.

day9_ex_test() ->
    %% takes no input and produces a copy of itself as output.
    P1 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99],
    P1 = run_io(P1, []),

    %% should output a 16-digit number.
    P2 = [1102,34915192,34915192,7,4,7,99,0],
    [1219070632396864] = run_io(P2, []),

    %% should output the large number in the middle.
    P3 = [104,1125899906842624,99],
    [1125899906842624] = run_io(P3, []),
    ok.
