-module(aoc2023_day22).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"2023/data/day22_ex.txt", star1, 5},
        {"2023/data/day22_ex.txt", star2, 7}
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
    % Verrify the assumption that the points are sorted in each dimention.
    Sizes = [size(C1, C2) || {C1, C2} <- Data],
    true = lists:all(fun all_pos/1, Sizes),

    Sorted = lists:sort(fun cmp/2, Data),
    Bricks = [make_brick(N, B) || {N, B} <- lists:enumerate(Sorted)],
    Supports = move(Bricks, [], #{}, []),
    Singles = lists:usort([S || [S] <- Supports, is_integer(S)]),
    length(Data) - length(Singles).

star2(Data) ->
    Sorted = lists:sort(fun cmp/2, Data),
    Bricks = [make_brick(N, B) || {N, B} <- lists:enumerate(Sorted)],
    Supports = move(Bricks, [], #{}, []),
    Singles = lists:usort([S || [S] <- Supports, is_integer(S)]),
    EnumeratedSupports = lists:enumerate(Supports),

    Counts = [count_falls([S], EnumeratedSupports) || S <- Singles],
    lists:sum(Counts).

read(File) ->
    lists:flatten([
        tools:group(2, tools:group(3, L))
     || L <- tools:read_format(File, "~d,~d,~d~~~d,~d,~d")
    ]).

size({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X2 - X1 + 1, Y2 - Y1 + 1, Z2 - Z1 + 1}.

all_pos({X, Y, Z}) when X >= 0, Y >= 0, Z >= 0 ->
    true;
all_pos(_) ->
    false.

cmp({{_, _, Z1}, _}, {{_, _, Z2}, _}) when Z1 < Z2 ->
    true;
cmp({{_, _, Z1}, _}, {{_, _, Z2}, _}) when Z1 > Z2 ->
    false;
cmp(C1, C2) ->
    C1 =< C2.

move([], _Out, _Pile, Supports) ->
    lists:reverse(Supports);
move([{1, Brick} = H | Rest], Out, Pile, Supports) ->
    move(Rest, [H | Out], maps:merge(Pile, Brick), [[ground] | Supports]);
move([{Zmin, Brick} = H | Rest], Out, Pile, Supports) ->
    case below(Brick, Pile) of
        [] ->
            Moved = #{{X, Y, Z - 1} => V || {X, Y, Z} := V <- Brick},
            move([{Zmin - 1, Moved} | Rest], Out, Pile, Supports);
        Sups ->
            move(Rest, [H | Out], maps:merge(Pile, Brick), [Sups | Supports])
    end.

below(Brick, Pile) ->
    lists:usort([maps:get({X, Y, Z - 1}, Pile, 0) || {X, Y, Z} <- maps:keys(Brick)]) -- [0].

make_brick(N, {{X1, Y1, Z1}, {X2, Y2, Z2}}) ->
    Points = [{X, Y, Z} || X <- lists:seq(X1, X2), Y <- lists:seq(Y1, Y2), Z <- lists:seq(Z1, Z2)],
    {Z1, maps:from_keys(Points, N)}.

count_falls(Falling, []) ->
    length(Falling) - 1;
count_falls(Falling, [{N, Supports} | Rest]) ->
    case Supports -- Falling of
        [] ->
            count_falls([N | Falling], Rest);
        _ ->
            count_falls(Falling, Rest)
    end.
