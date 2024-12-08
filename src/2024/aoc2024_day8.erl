-module(aoc2024_day8).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day8_ex.txt", star1, 14},
        {"examples/2024/day8_ex.txt", star2, 34}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 8},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    Groups = group([{F, Pos} || Pos = {_, _} := F <- Map, F /= $.], #{}),
    An = lists:flatten([all_antinodes(As, []) || _ := As <- Groups]),
    Max = maps:get(max, Map),
    Antinodes = [Pos || Pos <- lists:usort(An), inbound(Pos, Max)],
    %  Am = #{Pos => $# || Pos <- Antinodes},
    % tools:print_grid(maps:merge(Map, Am)),

    length(Antinodes).

star2(Map) ->
    Groups = group([{F, Pos} || Pos = {_, _} := F <- Map, F /= $.], #{}),
    Max = maps:get(max, Map),
    An = lists:flatten([all_antinodes2(As, [], Max) || _ := As <- Groups]),
    Antinodes = [Pos || Pos <- lists:usort(An), inbound(Pos, Max)],
    % Am = #{Pos => $# || Pos <- Antinodes},
    % tools:print_grid(maps:merge(Map, Am)),

    length(Antinodes).

read(File) ->
    tools:read_grid(File).

group([], Groups) ->
    Groups;
group([{F, Pos} | Rest], Groups) ->
    group(Rest, Groups#{F => [Pos | maps:get(F, Groups, [])]}).

all_antinodes([_], Res) ->
    lists:flatten(Res);
all_antinodes([A | Rest], Res) ->
    New = [antinodes(A, B) || B <- Rest],
    all_antinodes(Rest, [New | Res]).

antinodes(P1, P2) ->
    Dxy = aoc_vector:sub(P1, P2),
    [aoc_vector:sub(P2, Dxy), aoc_vector:add(P1, Dxy)].

inbound({X, Y}, {Xmax, Ymax}) ->
    X =< Xmax andalso Y =< Ymax andalso X >= 0 andalso Y >= 0.

all_antinodes2([_], Res, _Max) ->
    lists:flatten(Res);
all_antinodes2([A | Rest], Res, Max) ->
    New = [antinodes2(A, B, Max) || B <- Rest],
    all_antinodes2(Rest, [New | Res], Max).

antinodes2(P1, P2, Max) ->
    Dxy = aoc_vector:sub(P1, P2),
    [
        P1,
        P2,
        generate_while_inbound(P1, Dxy, Max, []),
        generate_while_inbound(P2, aoc_vector:mul(-1, Dxy), Max, [])
    ].

generate_while_inbound(Pos, Dxy, Max, Acc) ->
    Next = aoc_vector:add(Pos, Dxy),
    case inbound(Next, Max) of
        true ->
            generate_while_inbound(Next, Dxy, Max, [Next | Acc]);
        false ->
            Acc
    end.
