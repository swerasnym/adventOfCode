-module(day3).

-export([run/2]).

%% --- Day 3: Toboggan Trajectory ---
%%
%% With the toboggan login problems resolved, you set off toward the
%% airport. While travel by toboggan might be easy, it's certainly not safe:
%% there's very minimal steering and the area is covered in trees. You'll need
%% to see which angles will take you near the fewest trees.
%%
%% Due to the local geology, trees in this area only grow on exact integer
%% coordinates in a grid. You make a map (your puzzle input) of the open squares
%% (.) and trees (#) you can see. For example:
%%
%% ..##.......
%% #...#...#..
%% .#....#..#.
%% ..#.#...#.#
%% .#...##..#.
%% ..#.##.....
%% .#.#.#....#
%% .#........#
%% #.##...#...
%% #...##....#
%% .#..#...#.#
%%
%% These aren't the only trees, though; due to something you read about once
%% involving arboreal genetics and biome stability, the same pattern repeats to
%% the right many times:
%%
%% ..##.........##.........##.........##.........##.........##.......  --->
%% #...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
%% .#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
%% ..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
%% .#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
%% ..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....  --->
%% .#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
%% .#........#.#........#.#........#.#........#.#........#.#........#
%% #.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
%% #...##....##...##....##...##....##...##....##...##....##...##....#
%% .#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
%%
%% You start on the open square (.) in the top-left corner and need to reach the
%% bottom (below the bottom-most row on your map).
%%
%% The toboggan can only follow a few specific slopes (you opted for a cheaper
%% model that prefers rational numbers); start by counting all the trees you
%% would encounter for the slope right 3, down 1:
%%
%% From your starting position at the top-left, check the position that is right
%% 3 and down 1. Then, check the position that is right 3 and down 1 from there,
%% and so on until you go past the bottom of the map.
%%
%% The locations you'd check in the above example are marked here with O where
%% there was an open square and X where there was a tree:
%%
%% ..##.........##.........##.........##.........##.........##.......  --->
%% #..O#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
%% .#....X..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
%% ..#.#...#O#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
%% .#...##..#..X...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
%% ..#.##.......#.X#.......#.##.......#.##.......#.##.......#.##.....  --->
%% .#.#.#....#.#.#.#.O..#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
%% .#........#.#........X.#........#.#........#.#........#.#........#
%% #.##...#...#.##...#...#.X#...#...#.##...#...#.##...#...#.##...#...
%% #...##....##...##....##...#X....##...##....##...##....##...##....#
%% .#..#...#.#.#..#...#.#.#..#...X.#.#..#...#.#.#..#...#.#.#..#...#.#  --->
%%
%% In this example, traversing the map using this slope would cause you to
%% encounter 7 trees.
%%
%% Starting at the top-left corner of your map and following a slope of right 3
%% and down 1, how many trees would you encounter?
%%
%% Your puzzle answer was 280.
%% --- Part Two ---
%%
%% Time to check the rest of the slopes - you need to minimize the probability
%% of a sudden arboreal stop, after all.
%%
%% Determine the number of trees you would encounter if, for each of the
%% following slopes, you start at the top-left corner and traverse the map all
%% the way to the bottom:
%%
%%     Right 1, down 1.
%%     Right 3, down 1. (This is the slope you already checked.)
%%     Right 5, down 1.
%%     Right 7, down 1.
%%     Right 1, down 2.
%%
%% In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s)
%% respectively; multiplied together, these produce the answer 336.
%%
%% What do you get if you multiply together the number of trees encountered on
%% each of the listed slopes?
%%
%% Your puzzle answer was 4355551200.
%%
%% Both parts of this puzzle are complete! They provide two gold stars: **

run(Star, File) ->
    Data = read(File),
    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

star1(Map) ->
    {_Xmax, Ymax} = maps:get(dim, Map),
    Results = [get_pos(Y * 3, Y, Map) || Y <- lists:seq(0, Ymax - 1)],
    count(Results).

star2(Map) ->
    {_Xmax, Ymax} = maps:get(dim, Map),
    #{tree := T1} = count([get_pos(Y * 1, Y, Map) || Y <- lists:seq(0, Ymax - 1)]),
    #{tree := T2} = count([get_pos(Y * 3, Y, Map) || Y <- lists:seq(0, Ymax - 1)]),
    #{tree := T3} = count([get_pos(Y * 5, Y, Map) || Y <- lists:seq(0, Ymax - 1)]),
    #{tree := T4} = count([get_pos(Y * 7, Y, Map) || Y <- lists:seq(0, Ymax - 1)]),
    #{tree := T5} = count([get_pos(N, N * 2, Map) || N <- lists:seq(0, Ymax div 2)]),
    T1 * T2 * T3 * T4 * T5.

read(File) ->
    {ok, Bin} = file:read_file(File),
    List = binary_to_list(Bin),
    read(List, 0, 0, #{}).

read([$\n], X, Y, Acc) ->
    Acc#{dim => {X, Y + 1}};
read([$\n | Rest], _X, Y, Acc) ->
    read(Rest, 0, Y + 1, Acc);
read([Char | Rest], X, Y, Acc) ->
    case Char of
        $# ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => tree});
        $. ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => open})
    end.

get_pos(X, Y, #{dim := {Xmax, _Ymax}} = Map) ->
    maps:get({X rem Xmax, Y}, Map).

count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).
