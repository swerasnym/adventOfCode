-module(aoc2019_day8).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 8},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Layers =
        [string:slice(Data, Pos, 25 * 6) || Pos <- lists:seq(0, string:length(Data) - 1, 25 * 6)],
    {_, Layer} = lists:min([{count(Layer, "0"), Layer} || Layer <- Layers]),
    count(Layer, "1") * count(Layer, "2").

star2(Data) ->
    Layers =
        [string:slice(Data, Pos, 25 * 6) || Pos <- lists:seq(0, string:length(Data) - 1, 25 * 6)],
    Image = #{{Pos rem 25, Pos div 25} => pixel(Pos, Layers) || Pos <- lists:seq(0, 25 * 6 - 1)},
    tools:print_grid(Image),
    aoc_ocr:decode(Image, $█).

read(File) ->
    {ok, Bin} = file:read_file(File),
    string:trim(Bin).

count(Layer, String) ->
    length(string:split(Layer, String, all)) - 1.

pixel(Pos, [Top | Rest]) ->
    case string:slice(Top, Pos, 1) of
        <<"2">> ->
            pixel(Pos, Rest);
        <<"1">> ->
            $█;
        _ ->
            $\s
    end.
