-module(aoc2018_day8).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day8_ex.txt", star1, 138},
        {"examples/2018/day8_ex.txt", star2, 66}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 8},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Stream) ->
    get_metadata_sum(Stream, [new], 0).

star2(Stream) ->
    get_value(Stream, [new], [[]]).

read(File) ->
    tools:read_integers(File).

get_metadata_sum([NoChildren, NoMeta | Stream], [new | States], MetaAcc) ->
    get_metadata_sum(Stream, [{NoChildren, NoMeta} | States], MetaAcc);
get_metadata_sum(Stream, [{0, NoMeta} | States], MetaAcc) ->
    {Meta, NewStream} = lists:split(NoMeta, Stream),
    get_metadata_sum(NewStream, States, MetaAcc + lists:sum(Meta));
get_metadata_sum(Stream, [{N, NoMeta} | States], MetaAcc) ->
    get_metadata_sum(Stream, [new, {N - 1, NoMeta} | States], MetaAcc);
get_metadata_sum([], [], MetaAcc) ->
    MetaAcc.
get_value([NoChildren, NoMeta | Stream], [new | States], ChildStack) ->
    get_value(Stream, [{NoChildren, NoMeta} | States], [[] | ChildStack]);
get_value(Stream, [{0, NoMeta} | States], [MyC, ParentC | ChildStack]) ->
    {Meta, NewStream} = lists:split(NoMeta, Stream),
    case MyC of
        [] ->
            Value = lists:sum(Meta);
        _ ->
            Children = lists:reverse(MyC),
            Value = lists:sum([lists:nth(N, Children) || N <- Meta, N =< length(Children)])
    end,
    get_value(NewStream, States, [[Value | ParentC] | ChildStack]);
get_value(Stream, [{N, NoMeta} | States], ChildStack) ->
    get_value(Stream, [new, {N - 1, NoMeta} | States], ChildStack);
get_value([], [], [[RootValue]]) ->
    RootValue.
