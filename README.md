# Advent Of Code — Using Erlang!
This is the place where I keep my [Advent of Code](https://adventofcode.com/) solutions written in [Erlang.](https://www.erlang.org/) It currently contains solutions for all problems for the years 2015, 2018..2023.


## Configuration
There exists support to automatically download inputs and check the correct results for solved puzzles, this requires the configuration file `aoc.config` to be created; this can be achieved simply by running:  
`./init.sh`.  
Doing so will also trigger the initial build.

To enable automatic downloads of inputs, edit the `{session_id, ""}` tuple in `aoc.config` with the value of your session cookie from the [Advent of Code](https://adventofcode.com/) website after you logged in.

If you wish to store the inputs somewhere outside of the repo you can! Just edit the `{base_path, "aoc_storage"}` tuple to match your desired path.

## Building
The project uses [Rebar3](https://rebar3.org/) so it can be built using the `rebar3 compile` command.

## Running
There exists a few things to run in this repo. How to run some of the more common things are layed out below.


### Run solutions
The tool `./solve.sh` is used to download all inputs and solve all problems by just running it on its own! If you fancy to just run a all problems for specific year this is done with `./solve.sh <year>`. If you want a particular day use `./solve.sh <year> <day>`.


To solve both stars with a **custom input file**, just run  
`./solve.sh <year> <day> both <file>`.

To do the same for a single star use  
`./solve.sh <year> <day> star1 <file>` or  
`./solve.sh <year> <day> star2 <file>`   
instead. Note that these are the only cases that does not require the setup to be done. 


### Run help functions
Some days a few helper functions have been created. These can be run by giving the corresponding erlang expression to `run.sh`. 

As an example to generate the Graphviz file (for the example input) used to solve 2023 day 25 just run:  
`./run.sh 'aoc2023_day25:input_to_graphviz("priv/examples/2023/day25_ex.txt").'`.

Note that using `./run.sh` requires the project to be built first.

### Start a shell
To get access to the full functionality a shell can be started using `rebar3 shell`. 

Where you as an example can run all examples from 2023 using the command:  
 `aoc_solution:run(2023, all, examples).`. Just be ware of the long internal result format that will show up at the end, just scroll up for the nicer version!

## The Solution structure
All solutions are implemented and run using the [custom behaviour](https://www.erlang.org/doc/design_principles/spec_proc#user-defined-behaviours) `aoc_solution` that requires each solution to export the four functions `info/0`, `read/1`, `star1/1`, and `star2/1`. Where `read/1` is responsible for converting the input to an internal format. This internal format is then passed on to `star1/1` and `star2/1` which solves the actual problem. The `info/0` function provides information about the year and day of the problem and a way to provide example inputs.

### Providing example inputs
Examples that can be checked automatically are provided as a list of tuples `{FileOrData, Star, Result}` where star is either a single atom `star1`/`star2` or a tuple `{Star, Parameter}` to allow for examples that use a different parameter than the main input. For the parameterized example the module has to export one of the optional callbacks `star1/2`, and `star2/2` taking the parameter as the second parameter.

The examples can be provided by giving the relative path from the `priv` folder. Alternatively the tuple `{data, Data}`, this bypasses the call to `read/1` and provides the data directly.

### Example solution
Below is an example of the simple problem:  
> Star1: Calculate the sum of the values in the file.  
> Star2: Calculate the sum of the reminders mod N. With N = 17 for the real input, but N = 2 and N = 3 in the examples.

```erlang
-module(aoc_example).
-behaviour(aoc_solution).

-export([info/0, star1/1, star2/1, star2/2, read/1]).

info() ->
    Data = [1, 2, 3, 4, 5, 6],

    Examples = [
        {{data, Data}, star1, 21},
        {"examples/example.txt", star1, 21},
        {"examples/example.txt", {star2, 2}, 3},
        {"examples/example.txt", {star2, 3}, 6}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {0, 0},
        examples => Examples
    }).

star1(Values) ->
    lists:sum(Values).

star2(Values) ->
    star2(Values, 17).

star2(Values, P) ->
    lists:sum([V rem P || V <- Values]).

read(File) ->
    tools:read_integers(File).
```