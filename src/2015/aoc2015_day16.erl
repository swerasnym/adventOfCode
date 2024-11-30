-module(aoc2015_day16).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2015/day16_ex.txt", star1, unknown},
        % {"examples/2015/day16_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 16},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    [Sue] = [S || {S, Info} <- Data, lists:all(fun check/1, Info)],
    Sue.

star2(Data) ->
    [Sue] = [S || {S, Info} <- Data, lists:all(fun check2/1, Info)],
    Sue.

read(File) ->
    tools:read_lines(File, fun parse_sue/1).

parse_sue(Line) ->
    {[Sue], Rest} = tools:parse_format(Line, "Sue ~d: ", rest),
    Info = string:split(Rest, ", ", all),
    Split = [string:split(I, ": ") || I <- Info],
    {Sue, [{erlang:list_to_existing_atom(K), erlang:list_to_integer(V)} || [K, V] <- Split]}.

scan_result() ->
    #{
        children => 3,
        cats => 7,
        samoyeds => 2,
        pomeranians => 3,
        akitas => 0,
        vizslas => 0,
        goldfish => 5,
        trees => 3,
        cars => 2,
        perfumes => 1
    }.

check({K, V}) ->
    V == maps:get(K, scan_result(), V).

check2({K, V}) when K == cats; K == trees ->
    V > maps:get(K, scan_result(), V);
check2({K, V}) when K == pomeranians; K == goldfish ->
    V < maps:get(K, scan_result(), V);
check2({K, V}) ->
    V == maps:get(K, scan_result(), V).
