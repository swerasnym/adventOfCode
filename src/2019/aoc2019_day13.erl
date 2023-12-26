-module(aoc2019_day13).
-behaviour(aoc_solution).

-record(state, {screen = #{}, ball = {0, 0}, paddle = 0, score = 0}).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 13},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Program) ->
    Options = [{outputpid, self()}],
    Pid = intcode:spawn(Program, Options),
    #state{screen = Screen} = arcade(#state{}, Pid),
    paint(Screen),
    length([X || X <- maps:values(Screen), X == block]).

star2(Program) ->
    Options = [{inputpid, self()}, {outputpid, self()}, {set, 2, 0}],
    Pid = intcode:spawn(Program, Options),
    #state{screen = Screen, score = Score} = arcade(#state{}, Pid),
    paint(Screen),
    Score.

read(File) ->
    intcode:from_file(File).

arcade(State, Pid) ->
    case intcode:recvn(Pid, 3, 1000) of
        {ok, [X, Y, Id]} ->
            State1 = output(State, X, Y, Id),
            arcade(State1, Pid);
        {input, []} ->
            State1 = input(State, Pid),
            arcade(State1, Pid);
        {halt, []} ->
            State
    end.

input(
    #state{
        screen = _Screen,
        paddle = {Px, _Py},
        ball = {Bx, _By}
    } =
        State,
    Pid
) ->
    Data =
        case Px - Bx of
            0 ->
                [0];
            Value when Value < 0 ->
                [1];
            Value when Value > 0 ->
                [-1]
        end,
    intcode:send(Pid, Data),
    State.

output(#state{} = State, -1, 0, Score) ->
    State#state{score = Score};
output(#state{screen = Screen} = State, X, Y, Id) ->
    case Id of
        0 ->
            State#state{screen = Screen#{{X, Y} => empty}};
        1 ->
            State#state{screen = Screen#{{X, Y} => wall}};
        2 ->
            State#state{screen = Screen#{{X, Y} => block}};
        3 ->
            State#state{screen = Screen#{{X, Y} => paddle}, paddle = {X, Y}};
        4 ->
            State#state{screen = Screen#{{X, Y} => ball}, ball = {X, Y}}
    end.

paint(Screen) ->
    Xs = [X || {X, _} <- maps:keys(Screen)],
    Ys = [Y || {_, Y} <- maps:keys(Screen)],

    [
        paint({X, Y}, Screen, lists:max(Xs))
     || Y <- lists:seq(lists:min(Ys), lists:max(Ys)),
        X <- lists:seq(lists:min(Xs), lists:max(Xs))
    ],
    ok.

paint({X, _} = Pos, Screen, X) ->
    case maps:get(Pos, Screen, black) of
        empty ->
            io:format(" ~n", []);
        wall ->
            io:format("%~n", []);
        block ->
            io:format("*~n", []);
        paddle ->
            io:format("|~n", []);
        ball ->
            io:format("o~n", [])
    end;
paint(Pos, Screen, _) ->
    case maps:get(Pos, Screen, black) of
        empty ->
            io:format(" ", []);
        wall ->
            io:format("%", []);
        block ->
            io:format("*", []);
        paddle ->
            io:format("_", []);
        ball ->
            io:format("o", [])
    end.
