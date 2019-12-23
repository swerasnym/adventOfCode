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
	    intcode:interactive(Program);
	_ ->
	    Star1 = star1(Program),
	    Star2 = star2(Program),
	    {Star1, Star2}
    end.

star1(Program) ->
    Code =
	"OR A J\n"
	"AND B J\n"
	"AND C J\n"
	"NOT J J\n"
	"AND D J\n"
	"WALK\n",
    Result = intcode:run(Program, [{input, Code}]),
    Output = intcode:get_output(Result),

    io:format("~s~s", [Code, lists:droplast(Output)]),
    lists:last(Output).

star2(Program) ->
    Code =
	"OR A T\n"
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


