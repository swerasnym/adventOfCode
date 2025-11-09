-module(aoc2017_day25).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day25_ex.txt", star1, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 25},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Start, Steps, Cards}) ->
    {_, _, Tape} = tools:repeat(Steps, step(Cards), {0, Start, #{}}),
    tools:count(1, Tape).
star2(_) ->
    {done, "Reboot the printer!"}.

read(File) ->
    String = tools:read_string(File),
    Cleaned0 = string:replace(String, ".", "", all),
    % eqwalizer:ignore [string()] is a string()...
    Cleaned1 = string:replace(Cleaned0, ":", "", all),
    Clean = lists:flatten(Cleaned1),
    [Header | Cards] = tools:parse_blocks(Clean),
    [InitialState, Steps] = tools:parse_format(
        Header, "Begin in state ~a\nPerform a diagnostic checksum after ~d steps"
    ),
    {InitialState, Steps, parse_cards(Cards)}.

parse_cards(Cards) ->
    maps:from_list([parse_card(C) || C <- Cards]).

parse_card("In state " ++ [S, $\n | Card]) ->
    Format =
        """
          If the current value is 0
            - Write the value ~d
            - Move one slot to the ~a
            - Continue with state ~a
          If the current value is 1
            - Write the value ~d
            - Move one slot to the ~a
            - Continue with state ~a
        """,
    [If0, If1] = tools:group(3, tools:parse_format(Card, Format)),
    % elp:ignore W0023 (atoms_exhaustion)
    {list_to_atom([S]), #{0 => If0, 1 => If1}}.

step(Cards) ->
    fun({Pos, State, Tape}) ->
        Card = maps:get(State, Cards),
        Read = maps:get(Pos, Tape, 0),
        {Write, Dir, Next} = maps:get(Read, Card),
        {move(Pos, Dir), Next, Tape#{Pos => Write}}
    end.

move(Pos, right) -> Pos - 1;
move(Pos, left) -> Pos + 1.
