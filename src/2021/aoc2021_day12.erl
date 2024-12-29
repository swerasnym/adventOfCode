-module(aoc2021_day12).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 12},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    paths(start, [], Data, true).

star2(Data) ->
    paths(start, [], Data, false).

read(File) ->
    erlang:erase(),
    Lines = tools:read_lines(File),
    Paths =
        [
            begin
                [T, F] = string:split(Line, "-"),
                Ta = list_to_atom(T),
                Fa = list_to_atom(F),
                erlang:put(Fa, small(F)),
                erlang:put(Ta, small(T)),
                [Fa, Ta]
            end
         || Line <- Lines
        ],
    Rooms = lists:umerge(Paths),
    maps:from_list([neighbours(Room, Paths, []) || Room <- Rooms]).

neighbours(Room, [], Acc) ->
    {Room, lists:delete(start, Acc)};
neighbours(Room, [[Room, N] | Rest], Acc) ->
    neighbours(Room, Rest, [N | Acc]);
neighbours(Room, [[N, Room] | Rest], Acc) ->
    neighbours(Room, Rest, [N | Acc]);
neighbours(Room, [_ | Rest], Acc) ->
    neighbours(Room, Rest, Acc).

paths('end', _Visited, _Nerbours, _) ->
    1;
paths(Pos, Visited, Neighbours, true) ->
    case small_visited(Pos, Visited) of
        true ->
            0;
        false ->
            Visited1 = visit(Pos, Visited),
            lists:sum([paths(N, Visited1, Neighbours, true) || N <- maps:get(Pos, Neighbours)])
    end;
paths(Pos, Visited, Neighbours, false) ->
    Repeat = small_visited(Pos, Visited),
    Visited1 = visit(Pos, Visited),
    lists:sum([paths(N, Visited1, Neighbours, Repeat) || N <- maps:get(Pos, Neighbours)]).

small_visited(Pos, Visited) ->
    erlang:get(Pos) and lists:member(Pos, Visited).

small(Pos) when is_atom(Pos) ->
    erlang:get(Pos);
small(Pos) ->
    string:to_lower(Pos) == Pos.

visit(Pos, Visited) ->
    case erlang:get(Pos) of
        true ->
            [Pos | Visited];
        false ->
            Visited
    end.
