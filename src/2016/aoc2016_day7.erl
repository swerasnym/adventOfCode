-module(aoc2016_day7).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day7_ex.txt", star1, 2},
        {"examples/2016/day7_ex.txt", star2, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 7},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Adresses) ->
    length([Ipv7 || Ipv7 <- Adresses, tls_suport(Ipv7)]).

star2(Adresses) ->
    length([Ipv7 || Ipv7 <- Adresses, ssl_suport(Ipv7)]).

read(File) ->
    tools:read_lines(File).

tls_suport(Ipv7) ->
    tls_support(Ipv7, positive, false).

ssl_suport(Ipv7) ->
    ssl_support(Ipv7, positive, {[], []}).

% Use macros since these characters broke syntax highlighting...
-define(O, $[).
-define(C, $]).

tls_support(List, _, Res) when length(List) < 4 ->
    Res;
tls_support([?O | Rest], _, Res) ->
    tls_support(Rest, negative, Res);
tls_support([?C | Rest], _, Res) ->
    tls_support(Rest, positive, Res);
tls_support([A, A, A | [A | _] = Rest], Type, Res) ->
    tls_support(Rest, Type, Res);
tls_support([A, B, B, A | _], negative, _) ->
    false;
tls_support([A, B, B, A | Rest], positive, _) ->
    tls_support(Rest, positive, true);
tls_support([_ | Rest], Type, Res) ->
    tls_support(Rest, Type, Res).

ssl_support(List, _, {Pres, Nres}) when length(List) < 3 ->
    length(tools:overlap(lists:sort(Pres), lists:sort(Nres))) > 0;
ssl_support([?O | Rest], _, Res) ->
    ssl_support(Rest, negative, Res);
ssl_support([?C | Rest], _, Res) ->
    ssl_support(Rest, positive, Res);
ssl_support([A, A | [A | _] = Rest], Type, Res) ->
    ssl_support(Rest, Type, Res);
ssl_support([A | [B, A | _] = Rest], negative, {Pres, Nres}) ->
    ssl_support(Rest, negative, {Pres, [[B, A, B] | Nres]});
ssl_support([A | [B, A | _] = Rest], positive, {Pres, Nres}) ->
    ssl_support(Rest, positive, {[[A, B, A] | Pres], Nres});
ssl_support([_ | Rest], Type, Res) ->
    ssl_support(Rest, Type, Res).
