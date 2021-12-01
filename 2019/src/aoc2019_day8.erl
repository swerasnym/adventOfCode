-module(aoc2019_day8).

-export([run/2]).

run(Star, File) ->
    {ok, Bin} = file:read_file(File),
    Data = string:trim(Bin),
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

star1(Data) ->
    Layers =
        [string:slice(Data, Pos, 25 * 6) || Pos <- lists:seq(0, string:length(Data) - 1, 25 * 6)],
    {_, Layer} = lists:min([{count(Layer, "0"), Layer} || Layer <- Layers]),
    count(Layer, "1") * count(Layer, "2").

star2(Data) ->
    Layers =
        [string:slice(Data, Pos, 25 * 6) || Pos <- lists:seq(0, string:length(Data) - 1, 25 * 6)],
    Image = [pixel(Pos, Layers) || Pos <- lists:seq(0, 25 * 6 - 1)],
    [io:fwrite("~s~n", [string:slice(Image, Pos, 25)])
     || Pos <- lists:seq(0, string:length(Image) - 1, 25)],

    "ACKPZ". % manual decode!

count(Layer, String) ->
    length(string:split(Layer, String, all)) - 1.

pixel(Pos, [Top | Rest]) ->
    case string:slice(Top, Pos, 1) of
        <<"2">> ->
            pixel(Pos, Rest);
        <<"1">> ->
            "8";
        _ ->
            " "
    end.
