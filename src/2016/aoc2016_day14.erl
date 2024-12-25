-module(aoc2016_day14).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {{data, "abc"}, star1, 22728},
        {{data, "abc"}, {star2, 0}, 22728},
        {{data, "abc"}, {star2, 5}, 20353}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 14},
        examples => Examples,
        unit_test_input => false,
        unit_test_examples => true
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Base) ->
    Pads = find_pads(Base, 64, [{P, md5(Base, P)} || P <- lists:seq(0, 1000)], [], fun md5/2),
    hd(Pads).
star2(Base) ->
    star2(Base, 2016).

star2(Base, Repeats) ->
    MD5 = md5_s(Repeats),
    Pads = find_pads(Base, 64, [{P, MD5(Base, P)} || P <- lists:seq(0, 1000)], [], MD5),
    hd(Pads).

read(File) ->
    tools:read_string(File).

md5(String) ->
    [to_char(N) || <<N:4>> <= crypto:hash(md5, String)].

md5(String, Pad) ->
    [to_char(N) || <<N:4>> <= crypto:hash(md5, String ++ integer_to_list(Pad))].

md5_s(Repeats) ->
    fun(String, Pad) ->
        tools:repeat(Repeats, fun md5/1, md5(String, Pad))
    end.

to_char(N) when N < 10 ->
    $0 + N;
to_char(N) ->
    N - 10 + $a.

three_in_a_row(List) when length(List) < 3 ->
    {x, false};
three_in_a_row([A, A, A | _]) ->
    {A, true};
three_in_a_row([_ | Rest]) ->
    three_in_a_row(Rest).

five_in_a_row(x, _) ->
    false;
five_in_a_row(_, List) when length(List) < 5 ->
    false;
five_in_a_row(A, [A, A, A, A, A | _]) ->
    true;
five_in_a_row(A, [_ | Rest]) ->
    five_in_a_row(A, Rest).

find_pads(_, 0, _, Results, _) ->
    Results;
find_pads(Base, N, [{Pad, Hash} | Rest], Results, MD5) ->
    {C, Three} = three_in_a_row(Hash),
    case Three andalso lists:any(fun({_, H}) -> five_in_a_row(C, H) end, Rest) of
        true ->
            find_pads(
                Base, N - 1, Rest ++ [{Pad + 1001, MD5(Base, Pad + 1001)}], [Pad | Results], MD5
            );
        false ->
            find_pads(Base, N, Rest ++ [{Pad + 1001, MD5(Base, Pad + 1001)}], Results, MD5)
    end.
