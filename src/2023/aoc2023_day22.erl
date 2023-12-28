-module(aoc2023_day22).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2023/day22_ex.txt", star1, 5},
        {"examples/2023/day22_ex.txt", star2, 7}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 22},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    % Verify the assumption that the points are sorted in each dimension.
    true = lists:all(fun ordered/1, Data),

    Bricks = [make_brick(N, B) || {N, B} <- lists:enumerate(Data)],
    Supports = move(lists:sort(Bricks), #{}, []),
    Singles = lists:usort([S || {_, [S]} <- Supports, is_integer(S)]),
    length(Data) - length(Singles).

star2(Data) ->
    Bricks = [make_brick(N, B) || {N, B} <- lists:enumerate(Data)],
    Supports = move(lists:sort(Bricks), #{}, []),
    Singles = lists:usort([S || {_, [S]} <- Supports, is_integer(S)]),

    Counts = [count_falls([S], Supports) || S <- Singles],
    lists:sum(Counts).

read(File) ->
    [erlang:list_to_tuple(L) || L <- tools:read_multiple_formats(File, "~d,~d,~d~~~d,~d,~d")].

ordered({X1, Y1, Z1, X2, Y2, Z2}) when X1 =< X2, Y1 =< Y2, Z1 =< Z2 ->
    true;
ordered(_) ->
    false.

move([], _Pile, Supports) ->
    lists:reverse(Supports);
move([{1, N, Brick} | Rest], Pile, Supports) ->
    move(Rest, maps:merge(Pile, Brick), [{N, [ground]} | Supports]);
move([{Zmin, N, Brick} | Rest], Pile, Supports) ->
    case below(Brick, Pile) of
        [] ->
            Moved = #{{X, Y, Z - 1} => V || {X, Y, Z} := V <- Brick},
            move([{Zmin - 1, N, Moved} | Rest], Pile, Supports);
        Sups ->
            move(Rest, maps:merge(Pile, Brick), [{N, Sups} | Supports])
    end.

below(Brick, Pile) ->
    lists:usort([maps:get({X, Y, Z - 1}, Pile, void) || {X, Y, Z} <- maps:keys(Brick)]) -- [void].

make_brick(N, {X1, Y1, Z1, X2, Y2, Z2}) ->
    Points = [{X, Y, Z} || X <- lists:seq(X1, X2), Y <- lists:seq(Y1, Y2), Z <- lists:seq(Z1, Z2)],
    {Z1, N, maps:from_keys(Points, N)}.

count_falls(Falling, []) ->
    length(Falling) - 1;
count_falls(Falling, [{N, Supports} | Rest]) ->
    case Supports -- Falling of
        [] ->
            count_falls([N | Falling], Rest);
        _ ->
            count_falls(Falling, Rest)
    end.
