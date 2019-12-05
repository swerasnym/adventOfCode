-module(day5).
-export([run/2]).

run(Star, File) ->
    Program = intcode:from_file(File),
    case Star of
	star1 ->
	    star1(Program);
	star2 ->
	    star2(Program);
	_ -> 
	    Star1 = star1(Program),
	    Star2 = star2(Program),
	    {Star1, Star2}
    end.

star1(Program0) ->
    Program = intcode:set_input([1], Program0),
    Result = intcode:run(Program),
    intcode:get_output(Result).

star2(Program0) ->
    Program = intcode:set_input([5], Program0),
    Result = intcode:run(Program),
    intcode:get_output(Result).


