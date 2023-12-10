-module(advent_tests).

-include_lib("eunit/include/eunit.hrl").

run(Day, Star) ->
    [{Day, Star, _Time, Result}] = advent2020:run(Day, Star),
    Result.

day1_test() ->
    ?assertEqual(955584, run(aoc2020_day1, star1)),
    ?assertEqual(287503934, run(aoc2020_day1, star2)).

day2_test() ->
    ?assertEqual(524, run(aoc2020_day2, star1)),
    ?assertEqual(485, run(aoc2020_day2, star2)).

day3_test() ->
    ?assertEqual(280, run(aoc2020_day3, star1)),
    ?assertEqual(4355551200, run(aoc2020_day3, star2)).

day4_test() ->
    ?assertEqual(242, run(aoc2020_day4, star1)),
    ?assertEqual(186, run(aoc2020_day4, star2)).

day5_test() ->
    ?assertEqual(894, run(aoc2020_day5, star1)),
    ?assertEqual(579, run(aoc2020_day5, star2)).

day6_test() ->
    ?assertEqual(6683, run(aoc2020_day6, star1)),
    ?assertEqual(3122, run(aoc2020_day6, star2)).

day7_test() ->
    ?assertEqual(185, run(aoc2020_day7, star1)),
    ?assertEqual(89084, run(aoc2020_day7, star2)).

day8_test() ->
    ?assertEqual(1394, run(aoc2020_day8, star1)),
    ?assertEqual(1626, run(aoc2020_day8, star2)).

day9_test() ->
    ?assertEqual(14144619, run(aoc2020_day9, star1)),
    ?assertEqual(1766397, run(aoc2020_day9, star2)).

day10_test() ->
    ?assertEqual(1917, run(aoc2020_day10, star1)),
    ?assertEqual(113387824750592, run(aoc2020_day10, star2)).

day11_test() ->
    ?assertEqual(2386, run(aoc2020_day11, star1)),
    ?assertEqual(2091, run(aoc2020_day11, star2)).

day12_test() ->
    ?assertEqual(1294, run(aoc2020_day12, star1)),
    ?assertEqual(20592, run(aoc2020_day12, star2)).

day13_test() ->
    ?assertEqual(138, run(aoc2020_day13, star1)),
    ?assertEqual(226845233210288, run(aoc2020_day13, star2)).

day14_test() ->
    ?assertEqual(6513443633260, run(aoc2020_day14, star1)),
    ?assertEqual(3442819875191, run(aoc2020_day14, star2)).

day15_test() ->
    ?assertEqual(447, run(aoc2020_day15, star1)),
    ?assertEqual(11721679, run(aoc2020_day15, star2)).

day16_test() ->
    ?assertEqual(19060, run(aoc2020_day16, star1)),
    ?assertEqual(953713095011, run(aoc2020_day16, star2)).

day17_test() ->
    ?assertEqual(386, run(aoc2020_day17, star1)),
    ?assertEqual(2276, run(aoc2020_day17, star2)).

day18_test() ->
    ?assertEqual(654686398176, run(aoc2020_day18, star1)),
    ?assertEqual(8952864356993, run(aoc2020_day18, star2)).

day19_test() ->
    ?assertEqual(190, run(aoc2020_day19, star1)),
    ?assertEqual(311, run(aoc2020_day19, star2)).

day20_test() ->
    ?assertEqual(32287787075651, run(aoc2020_day20, star1)),
    ?assertEqual(1939, run(aoc2020_day20, star2)).

day21_test() ->
    ?assertEqual(1882, run(aoc2020_day21, star1)),
    ?assertEqual("xgtj,ztdctgq,bdnrnx,cdvjp,jdggtft,mdbq,rmd,lgllb", run(aoc2020_day21, star2)).

day22_test() ->
    ?assertEqual(34255, run(aoc2020_day22, star1)),
    ?assertEqual(33369, run(aoc2020_day22, star2)).

day23_test() ->
    ?assertEqual(98645732, run(aoc2020_day23, star1)),
    ?assertEqual(689500518476, run(aoc2020_day23, star2)).

day24_test() ->
    ?assertEqual(438, run(aoc2020_day24, star1)),
    ?assertEqual(4038, run(aoc2020_day24, star2)).

day25_test() ->
    ?assertEqual(18433997, run(aoc2020_day25, star1)),
    ?assertEqual("Pay Deposit!", run(aoc2020_day25, star2)).
