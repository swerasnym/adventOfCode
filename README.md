# Advent Of Code (In Erlang)
This is the place where I keep my [Advent of Ccode](https://adventofcode.com/) solutions written in [Erlang.](https://www.erlang.org/) It currently contains solutions for the years 2019 to 2023.


## Cofiguration
There exists support to automatically download inputs and check the correct results for solved puzzles. 

This requires a bit of configuration with instructions yet to be written.


## Building
The project uses [Rebar3](https://rebar3.org/) so it can be built using the `rebar3 compile` command.

## Running

### Both stars  
`./solve.sh <year> <day> both <file>`.

### Single star
`./solve.sh <year> <day> star1 <file>` or 
`./solve.sh <year> <day> star2 <file>`.


### Help functions
Some days a few helpers that write their output to standard out have been crated. These can be run by givint the corresponding exptession to `run.sh` for example 
`./run.sh 'aoc2023_day25:input_to_graphviz("priv/examples/2023/day25_ex.txt").'`.

### Shell
To get access to the full functionallty a shell can be started using `rebar3 shell`. Where you as an example can run all examples from 2023 using the command `aoc_solution:run(2023, all, examples).`.


## The Solution structure
All solutions are implemented and run using the [custom behaviour](https://www.erlang.org/doc/design_principles/spec_proc#user-defined-behaviours) `aoc_solution` that requires each solution to export the four functions `info/0`, `read/1`, `star1/1`, and `star2/1`. Where `read/1` is responsible for converting the input to an internal format. This internal format is then passed on to `star1/1` and `star2/1` which solves the actuall problem. The `info/0` function provides information about the year and day of the problem and a way to provide example inputs.



### Providing examples
Examples that can be checked automatcally are provided as a list of tuples `{File, Star, Result}` where star is either a single atom `star1`/`star2` or a tuple `{Star, Parameter}` to allow for examples that use a differerent patameter than the main input. For the parameterized example the one of the corresponding functions  

An example :

```erlang
-module(example).
-behaviour(aoc_solution).

-export([info/0, star1/1, star2/1, star2/2, read/1]).

info() ->
    Data = [1, 2, 3, 4, 5, 6],

    Examples = [
        {{data, Data}, star1, 21}
        {"examples/2023/example.txt", star1, 21},
        {"examples/2023/example.txt", {star2, 2}, 3},
        {"examples/2023/example.txt", {star2, 3}, 6}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 11},
        examples => Examples
    }).



star1(Values) ->
    lists:sum(Values).

star2(Data) ->
    star2(Data, 1000000).

star2(Values, P) ->
    lists:sum([V rem P || V <- Values]).

read(File) ->
    tools:read_integers(File).
```