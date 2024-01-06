-module(aoc2015_day4).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Data = {data, "abcdef"},

    Examples = [
        {Data, star1, 609043}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 4},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Secret) ->
    io:format("~s~n", [Secret]),
    check_parallel(Secret, "00000", 100, 1000).

star2(Secret) ->
    check_parallel(Secret, "000000", 100, 1000).

read(File) ->
    tools:read_string(File).

check(Secret, Number, Prefix) ->
    Hex = hex_string(erlang:md5(Secret ++ integer_to_list(Number))),
    lists:prefix(Prefix, Hex).

helper(Secret, Prefix, Base, Range, Step, Pid) ->
    Checked = [N || N <- lists:seq(Base, Base + Range), check(Secret, N, Prefix)],
    case length(Checked) of
        0 ->
            receive
                Number when Number < Base ->
                    Pid ! {self(), done};
                Number ->
                    self() ! Number,
                    helper(Secret, Prefix, Base + Step, Range, Step, Pid)
            after 0 ->
                helper(Secret, Prefix, Base + Step, Range, Step, Pid)
            end;
        _ ->
            Pid ! {self(), hd(Checked)}
    end.

check_parallel(Secret, Prefix, N, Range) ->
    Step = N * Range,
    Pid = self(),
    Helpers = [
        erlang:spawn_link(fun() -> helper(Secret, Prefix, Base, Range - 1, Step, Pid) end)
     || Base <- lists:seq(0, Step - Range, Range)
    ],
    receive
        {From, Number} ->
            [H ! Number || H <- Helpers, H /= From],
            Extra = [recv(H) || H <- Helpers, H /= From],
            Solutions = [Res || Res <- [Number | Extra], Res /= done],
            io:format("~p~n", [Solutions]),
            lists:min(Solutions)
    end.

recv(Pid) ->
    receive
        {Pid, Resp} ->
            Resp
    after 10000 ->
        error(timeout)
    end.

hex_string(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X])).
