-module(day21).
-export([run/2]).

run(Star, File) ->
    Program = intcode:from_file(File),
    case Star of
	star1 ->
	    star1(Program);
	star2 ->
	    star2(Program);
	interactive ->
	    interactive(Program);
	_ ->
	    Star1 = star1(Program),
	    Star2 = star2(Program),
	    {Star1, Star2}
    end.

star1(Program) ->
    Code =
	"NOT J J\n"
	"NOT T T\n"
	"AND A T\n"
	"AND B T\n"
	"AND C T\n"
	"NOT T T\n"
	"AND D T\n"
	"AND T J\n"
	"WALK\n",
    Result = intcode:run(Program, [{input, Code}]),
    Output = intcode:get_output(Result),

    io:format("~s~s", [Code, lists:droplast(Output)]),
    lists:last(Output).



star2(Program) ->
    Code =
	"NOT T T\n"
	"AND A T\n"
	"AND B T\n"
	"AND C T\n"
	"NOT T T\n"
	"OR E J\n"
	"OR H J\n"
	"AND D J\n"
	"AND T J\n"
	"RUN\n",
    Result = intcode:run(Program, [{input, Code}]),
    Output = intcode:get_output(Result),

    io:format("~s~s", [Code, lists:droplast(Output)]),
    lists:last(Output).




interactive(Program) ->
    Pid = intcode:spawn(Program, [{inputpid, self()}, {outputpid, self()}, {exitpid, self()}, {timeout, 60000}]),
    shell(Pid).




shell(Pid) ->
    case intcode:recvn(Pid, all) of
	{input, Prompt} ->
	    Line =io:get_line(Prompt),
	    intcode:send(Pid, Line),
	    shell(Pid);
	{halt, Output} ->
	    io:format("~s~nResult: ~p~n", [lists:droplast(Output), lists:last(Output)]),
	    lists:last(Output)
    end.
