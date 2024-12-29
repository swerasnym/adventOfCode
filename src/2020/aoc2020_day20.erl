-module(aoc2020_day20).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 20},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Images) ->
    Count =
        tools:count(lists:flatten([maps:values(Edges) || {_Id, {Edges, _Image}} <- Images])),
    Counrers = [Id || {Id, {Edges, _Image}} <- Images, is_corner(maps:values(Edges), Count)],
    product(Counrers).

star2(Images) ->
    Count =
        tools:count(lists:flatten([maps:values(Edges) || {_Id, {Edges, _Image}} <- Images])),
    Counrers = [Id || {Id, {Edges, _Image}} <- Images, is_corner(maps:values(Edges), Count)],
    Sides = lists:flatten([reorder(Edges, Id) || {Id, {Edges, _Image}} <- Images]),

    {FirstEdges, _FirstImage} = proplists:get_value(hd(Counrers), Images),

    Internal =
        [
            Side
         || {Side, Value} <- maps:to_list(FirstEdges),
            maps:get(Value, Count) == 1,
            Side /= top_rev,
            Side /= bot_rev,
            Side /= left_rev,
            Side /= right_rev
        ],

    Neighbours = build(Internal, hd(Counrers), Images, Sides, Count),
    Grid = orient(Neighbours, Images, Sides, Count),

    Highlighted = highlight_mosters(commbind(Grid)),
    tools:print_grid(Highlighted),
    tools:count($~, Highlighted).

read(File) ->
    [process(Image) || Image <- tools:read_blocks(File)].

process("Tile " ++ ImageMeta) ->
    {Id, ":\n" ++ Image} = string:to_integer(ImageMeta),
    ImageMap = tools:parse_grid(Image, #{$# => $1, $. => $0}),
    Edges = get_edges(ImageMap),
    {Id, {Edges, ImageMap}}.

highlight_mosters(Grid) ->
    highlight_mosters(get_tansforms(Grid), make_moster()).

highlight_mosters([], _Monster) ->
    no_monsters_found;
highlight_mosters([#{max := {Xmax, Ymax}} = Map | Maps], #{max := {Mx, My}} = Monster) ->
    Positions =
        [
            {X, Y}
         || X <- lists:seq(0, Xmax - Mx - 1),
            Y <- lists:seq(0, Ymax - My - 1),
            is_moster({X, Y}, Map, Monster)
        ],

    case length(Positions) of
        0 ->
            highlight_mosters(Maps, Monster);
        _ ->
            highlight(Positions, Monster, Map)
    end.

highlight([], _Monster, Map) ->
    tools:replace(Map, #{$0 => $., $1 => $~});
highlight([{X, Y} | Positions], Monster, Map) ->
    Highlights = maps:from_list([{{X + Mx, Y + My}, $O} || {Mx, My} <- maps:keys(Monster)]),
    highlight(Positions, Monster, maps:merge(Map, Highlights)).

is_moster({X, Y}, Grid, Monster) ->
    Match = [$# || {Mx, My} <- maps:keys(Monster), maps:get({X + Mx, Y + My}, Grid) == $1],
    length(Match) == maps:size(Monster) - 1.

make_moster() ->
    MonsterImage = "..................#.\n#....##....##....###\n.#..#..#..#..#..#...",
    maps:filter(fun(_K, V) -> V /= $. end, tools:parse_grid(MonsterImage)).

commbind(#{{0, 0} := #{max := {Xmax, Ymax}}} = Grid) ->
    List =
        [
            {{Gx * (Xmax - 1) + X - 1, Gy * (Ymax - 1) + Y - 1}, Value}
         || {{Gx, Gy}, Image} <- maps:to_list(Grid),
            {{X, Y}, Value} <- maps:to_list(Image),
            X /= 0,
            Y /= 0,
            X /= Xmax,
            Y /= Xmax
        ],
    Map = maps:from_list(List),
    {Mx, My} = lists:max(maps:keys(Map)),
    Map#{max => {Mx, My}}.

orient(Neighbours, Images, Sides, Count) ->
    Grid = to_grid(Neighbours),
    {Gx, Gy} = lists:max(maps:keys(Grid)),
    orient({0, 0}, Grid, Images, Sides, Count, #{}, {Gx + 1, Gy + 1}).

orient({_X, Ymax}, _Grid, _Images, _Sides, _Count, Acc, {_Xmax, Ymax}) ->
    Acc;
orient({Xmax, Y}, Grid, Images, Sides, Count, Acc, {Xmax, Ymax}) ->
    orient({0, Y + 1}, Grid, Images, Sides, Count, Acc, {Xmax, Ymax});
orient({X, Y} = Pos, Grid, Images, Sides, Count, Acc, Max) ->
    Id = maps:get(Pos, Grid),

    {_Edges, Map} = proplists:get_value(Id, Images),
    [NewMap] =
        [
            Orientation
         || Orientation <- get_tansforms(Map),
            check_neighbours(Id, Pos, Orientation, Grid, Sides, Count)
        ],

    orient({X + 1, Y}, Grid, Images, Sides, Count, Acc#{Pos => NewMap}, Max).

check_neighbours(Id, {X, Y}, Map, Grid, Sides, Count) ->
    is_neighbour(Id, top(Map), maps:get({X, Y - 1}, Grid, edge), Sides, Count) and
        is_neighbour(Id, bot(Map), maps:get({X, Y + 1}, Grid, edge), Sides, Count) and
        is_neighbour(Id, left(Map), maps:get({X - 1, Y}, Grid, edge), Sides, Count) and
        is_neighbour(Id, right(Map), maps:get({X + 1, Y}, Grid, edge), Sides, Count).

get_tansforms(Grid) ->
    Flip = tools:flip_grid(Grid),
    [
        Grid,
        tools:rotate_grid(Grid, cw),
        tools:rotate_grid(Grid, ccw),
        tools:rotate_grid(tools:rotate_grid(Grid)),
        Flip,
        tools:rotate_grid(Flip, cw),
        tools:rotate_grid(Flip, ccw),
        tools:rotate_grid(tools:rotate_grid(Flip))
    ].

is_neighbour(_Id, Value, edge, _Sides, Count) ->
    maps:get(Value, Count) == 1;
is_neighbour(Id, Value, Neighbour, Sides, _Count) ->
    case [N || {N, _} <- proplists:get_all_values(Value, Sides), N /= Id] of
        [Neighbour] ->
            true;
        _ ->
            false
    end.

to_grid(List) ->
    to_grid(List, 0, #{}).

to_grid([], _Y, Map) ->
    Map;
to_grid([First | Rest], Y, Map) ->
    to_grid(Rest, Y + 1, to_grid(First, 0, Y, Map)).

to_grid([], _X, _Y, Map) ->
    Map;
to_grid([{Id, _Dir} | Rest], X, Y, Map) ->
    to_grid(Rest, X + 1, Y, Map#{{X, Y} => Id}).

opposed(top) ->
    bot;
opposed(bot) ->
    top;
opposed(left) ->
    right;
opposed(right) ->
    left;
opposed(top_rev) ->
    bot_rev;
opposed(bot_rev) ->
    top_rev;
opposed(left_rev) ->
    right_rev;
opposed(right_rev) ->
    left_rev.

flip(top) ->
    top_rev;
flip(bot) ->
    bot_rev;
flip(left) ->
    left_rev;
flip(right) ->
    right_rev;
flip(top_rev) ->
    top;
flip(bot_rev) ->
    bot;
flip(left_rev) ->
    left;
flip(right_rev) ->
    right.

build([E1, _E2], Id, Images, Sides, Count) ->
    Row1 = match_edges(E1, Id, Images, Sides, Count),

    [
        match_edges(
            get_new_internal_edge(proplists:get_value(RowId, Images), Count, Side),
            RowId,
            Images,
            Sides,
            Count
        )
     || {RowId, Side} <- Row1
    ].

match_edges(Edge, Id, Images, Sides, Count) ->
    {Edges, _Image} = proplists:get_value(Id, Images),
    Value = maps:get(opposed(Edge), Edges),
    case [V || V = {Ids, _} <- proplists:get_all_values(Value, Sides), Ids /= Id] of
        [{NextId, NextEdge}] ->
            [{Id, Edge} | match_edges(NextEdge, NextId, Images, Sides, Count)];
        [] ->
            [{Id, Edge}]
    end.

get_new_internal_edge({Edges, _Image}, Count, Current) ->
    [S, _Srev] =
        [
            Side
         || {Side, Value} <- maps:to_list(Edges),
            maps:get(Value, Count) == 1,
            Side /= Current,
            Side /= flip(Current),
            opposed(Side) /= Current,
            opposed(Side) /= flip(Current)
        ],
    S.

reorder(Map, Id) ->
    [{Val, {Id, Key}} || {Key, Val} <- maps:to_list(Map)].

product(List) ->
    product(List, 1).

product([A], Result) ->
    A * Result;
product([A | Rest], Result) ->
    product(Rest, Result * A).

is_corner(Edges, Map) ->
    % two edges flipped an not with no pairing
    4 == length([V || V <- Edges, maps:get(V, Map) == 1]).

get_edges(#{max := {Xmax, Ymax}} = ImageMap) ->
    #{
        top => list_to_integer([maps:get({X, 0}, ImageMap) || X <- lists:seq(0, Xmax)], 2),
        top_rev =>
            list_to_integer([maps:get({X, 0}, ImageMap) || X <- lists:seq(Xmax, 0, -1)], 2),
        bot => list_to_integer([maps:get({X, Ymax}, ImageMap) || X <- lists:seq(0, Xmax)], 2),
        bot_rev =>
            list_to_integer([maps:get({X, Ymax}, ImageMap) || X <- lists:seq(Xmax, 0, -1)], 2),
        left => list_to_integer([maps:get({0, Y}, ImageMap) || Y <- lists:seq(0, Ymax)], 2),
        left_rev =>
            list_to_integer([maps:get({0, Y}, ImageMap) || Y <- lists:seq(Ymax, 0, -1)], 2),
        right => list_to_integer([maps:get({Xmax, Y}, ImageMap) || Y <- lists:seq(0, Ymax)], 2),
        right_rev =>
            list_to_integer([maps:get({Xmax, Y}, ImageMap) || Y <- lists:seq(Ymax, 0, -1)], 2)
    }.

top(#{max := {Xmax, _Ymax}} = ImageMap) ->
    list_to_integer([maps:get({X, 0}, ImageMap) || X <- lists:seq(0, Xmax)], 2).

bot(#{max := {Xmax, Ymax}} = ImageMap) ->
    list_to_integer([maps:get({X, Ymax}, ImageMap) || X <- lists:seq(0, Xmax)], 2).

left(#{max := {_Xmax, Ymax}} = ImageMap) ->
    list_to_integer([maps:get({0, Y}, ImageMap) || Y <- lists:seq(0, Ymax)], 2).

right(#{max := {Xmax, Ymax}} = ImageMap) ->
    list_to_integer([maps:get({Xmax, Y}, ImageMap) || Y <- lists:seq(0, Ymax)], 2).
