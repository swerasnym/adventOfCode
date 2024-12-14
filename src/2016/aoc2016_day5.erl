-module(aoc2016_day5).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, "abc"}, star1, "18f47a30"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 5},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    crack(Data, 0, 8, []).

star2(Data) ->
    crack2(Data, 0, 8, #{}).

read(File) ->
    tools:read_string(File).

md5(Room) ->
    <<Interesting:20, C:4, D:4, _/bitstring>> = crypto:hash(md5, Room),
    {Interesting, C, D}.

crack(_String, _Suffix, 0, Acc) ->
    [to_char(N) || N <- lists:reverse(Acc)];
crack(String, Suffix, Times, Acc) ->
    case md5(String ++ integer_to_list(Suffix)) of
        {0, N, _} ->
            io:format("~p~n", [Suffix]),
            crack(String, Suffix + 1, Times - 1, [N | Acc]);
        _ ->
            crack(String, Suffix + 1, Times, Acc)
    end.

crack2(_String, _Suffix, 0, Acc) ->
    [to_char(N) || {_, N} <- lists:sort(maps:to_list(Acc))];
crack2(String, Suffix, Times, Acc) ->
    case md5([String, integer_to_list(Suffix)]) of
        {0, Pos, N} when Pos < 8 andalso not is_map_key(Pos, Acc) ->
            io:format("~p~n", [Suffix]),
            crack2(String, Suffix + 1, Times - 1, Acc#{Pos => N});
        _ ->
            crack2(String, Suffix + 1, Times, Acc)
    end.

to_char(N) when N < 10 ->
    $0 + N;
to_char(N) ->
    N - 10 + $a.
