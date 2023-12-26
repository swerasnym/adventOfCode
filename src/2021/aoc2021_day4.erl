-module(aoc2021_day4).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 4},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Numbers, Boards}) ->
    {Number, Board} = play_bingo(Numbers, Boards),
    Left = lists:filter(fun(V) -> V /= x end, maps:values(Board)),
    Number * lists:sum(Left).

star2({Numbers, Boards}) ->
    {Number, Board} = play_bingo2(Numbers, Boards),
    io:format("~p ~p", [Number, tools:grid_to_lists(Board)]),
    Left = lists:filter(fun(V) -> V /= x end, maps:values(Board)),
    Number * lists:sum(Left).

read(File) ->
    [NumbersL | BoardsL] = tools:read_blocks(File),
    Numbers = tools:parse_integers(NumbersL, ", "),
    Boards =
        [
            tools:drop_max(tools:lists_to_grid(tools:parse_format(Board, "~d ~d ~d ~d ~d")))
         || Board <- BoardsL
        ],
    {Numbers, Boards}.

mark_board(Number, Board) ->
    maps:map(fun(_K, V) -> mark(V, Number) end, Board).

mark(Number, Number) ->
    x;
mark(BN, _) ->
    BN.

play_bingo([Number | Numbers], BoardsIn) ->
    Boards = [mark_board(Number, Board) || Board <- BoardsIn],
    case lists:foldl(fun has_bingo/2, no, Boards) of
        no ->
            play_bingo(Numbers, Boards);
        Result ->
            {Number, Result}
    end.

play_bingo2([Number | Numbers], [Board]) ->
    Marked = mark_board(Number, Board),
    case has_bingo(Marked, no) of
        no ->
            play_bingo2(Numbers, [Marked]);
        _ ->
            {Number, Marked}
    end;
play_bingo2([Number | Numbers], BoardsIn) ->
    Boards = [mark_board(Number, Board) || Board <- BoardsIn],
    play_bingo2(
        Numbers,
        lists:filter(fun(Board) -> has_bingo(Board, no) /= Board end, Boards)
    ).

has_bingo(
    #{
        {0, 0} := x,
        {0, 1} := x,
        {0, 2} := x,
        {0, 3} := x,
        {0, 4} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(
    #{
        {1, 0} := x,
        {1, 1} := x,
        {1, 2} := x,
        {1, 3} := x,
        {1, 4} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(
    #{
        {2, 0} := x,
        {2, 1} := x,
        {2, 2} := x,
        {2, 3} := x,
        {2, 4} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(
    #{
        {3, 0} := x,
        {3, 1} := x,
        {3, 2} := x,
        {3, 3} := x,
        {3, 4} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(
    #{
        {4, 0} := x,
        {4, 1} := x,
        {4, 2} := x,
        {4, 3} := x,
        {4, 4} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(
    #{
        {0, 0} := x,
        {1, 0} := x,
        {2, 0} := x,
        {3, 0} := x,
        {4, 0} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(
    #{
        {0, 1} := x,
        {1, 1} := x,
        {2, 1} := x,
        {3, 1} := x,
        {4, 1} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(
    #{
        {0, 2} := x,
        {1, 2} := x,
        {2, 2} := x,
        {3, 2} := x,
        {4, 2} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(
    #{
        {0, 3} := x,
        {1, 3} := x,
        {2, 3} := x,
        {3, 3} := x,
        {4, 3} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(
    #{
        {0, 4} := x,
        {1, 4} := x,
        {2, 4} := x,
        {3, 4} := x,
        {4, 4} := x
    } =
        Board,
    no
) ->
    Board;
has_bingo(_Board, no) ->
    no;
has_bingo(_, Result) ->
    Result.
