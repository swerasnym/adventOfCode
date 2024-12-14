-module(aoc2024_day14).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2024/day14_ex.txt", {star1, {11, 7}}, 12},
        {"examples/2024/day14_ex.txt", {star2, {11, 7}}, 1}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 14},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    star1(Data, {101, 103}).

star1(Robots, {W, H} = Size) ->
    Pos = [move(100, PV, Size) || PV <- Robots],

    Q1 = count_in(Pos, {0, 0}, {W div 2, H div 2}),
    Q2 = count_in(Pos, {W - (W div 2), 0}, {W, H div 2}),
    Q3 = count_in(Pos, {W - (W div 2), H - (H div 2)}, {W, H}),
    Q4 = count_in(Pos, {0, H - (H div 2)}, {W div 2, H}),

    Q1 * Q2 * Q3 * Q4.

star2(Robots) ->
    star2(Robots, {101, 103}).

star2(Robots, Size) ->
    find_tree(1, Robots, Size).

print_tree(Map, Size) ->
    tools:print_grid(Map#{max => Size}).

read(File) ->
    [tools:group(2, L) || L <- tools:read_multiple_formats(File, "p=~d,~d v=~d,~d")].

find_tree(Steps, _Robots, {W, H}) when Steps == W * H + 1 ->
    none;
find_tree(Steps, Robots, Size) ->
    Positions = move_all(Steps, Robots, Size),
    Unique = lists:usort(Positions),
    case length(Unique) == length(Positions) of
        true ->
            print_tree(#{P => $# || P <- Positions}, Size),
            Steps;
        false ->
            find_tree(Steps + 1, Robots, Size)
    end.

move_all(Steps, Robots, Size) ->
    [move(Steps, PV, Size) || PV <- Robots].

move(Steps, [P, V], {W, H}) ->
    {X, Y} = aoc_vector:add(P, aoc_vector:mul(Steps, V)),
    {tools:mod(X, W), tools:mod(Y, H)}.

count_in(Pos, {Xmin, Ymin}, {Xmax, Ymax}) ->
    length([{X, Y} || {X, Y} <- Pos, X >= Xmin, X < Xmax, Y >= Ymin, Y < Ymax]).
