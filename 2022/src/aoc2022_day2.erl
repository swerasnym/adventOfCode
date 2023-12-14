-module(aoc2022_day2).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 2}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    tools:read_lines(File).

star1(Data) ->
    lists:sum([score(X) || X <- Data]).

star2(Data) ->
    lists:sum([score2(X) || X <- Data]).

%% A for Rock, B for Paper, and C for Scissors.
%% X for Rock, Y for Paper, and Z for Scissors.
%% 1 for Rock, 2 for Paper, and 3 for Scissors
%% 0 if you lost, 3 if the round was a draw, and 6 if you won
score("A X") ->
    3 + 1;
score("A Y") ->
    6 + 2;
score("A Z") ->
    0 + 3;
score("B X") ->
    0 + 1;
score("B Y") ->
    3 + 2;
score("B Z") ->
    6 + 3;
score("C X") ->
    6 + 1;
score("C Y") ->
    0 + 2;
score("C Z") ->
    3 + 3.

%% A for Rock, B for Paper, and C for Scissors.
%% X for Lose, Y for Draw, and Z for Win.
%% 1 for Rock, 2 for Paper, and 3 for Scissors
%% 0 if you lost, 3 if the round was a draw, and 6 if you won
score2("A X") ->
    0 + 3;
score2("A Y") ->
    3 + 1;
score2("A Z") ->
    6 + 2;
score2("B X") ->
    0 + 1;
score2("B Y") ->
    3 + 2;
score2("B Z") ->
    6 + 3;
score2("C X") ->
    0 + 2;
score2("C Y") ->
    3 + 3;
score2("C Z") ->
    6 + 1.
