-module(aoc2020_day16).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 16},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Fileds, _MyTicket, Tickets}) ->
    {_Lables, Ranges} = lists:unzip(Fileds),
    RangeList = lists:flatten(Ranges),
    InvalidValues =
        [Value || Value <- lists:flatten(Tickets), not verify_any(Value, RangeList)],
    lists:sum(InvalidValues).

star2({Fileds, MyTicket, Tickets}) ->
    {_Lables, Ranges} = lists:unzip(Fileds),
    RangeList = lists:flatten(Ranges),
    ValidTickets = [Ticket || Ticket <- Tickets, verify_all(Ticket, RangeList)],

    Values = [get_values(N, ValidTickets) || N <- lists:seq(1, length(MyTicket))],
    PossibleMatches = [find_positions(Values, Range) || Range <- Fileds],
    Order = get_order(PossibleMatches, [], []),

    tools:product([lists:nth(N, MyTicket) || {N, "departure" ++ _} <- Order]).

read(File) ->
    [Fields, "your ticket:\n" ++ MyTicket, "nearby tickets:\n" ++ NerbyTickets] =
        tools:read_blocks(File),
    {
        [process_field(Field) || Field <- tools:parse_lines(Fields)],
        tools:parse_integers(MyTicket, ","),
        [tools:parse_integers(Ticket, ",") || Ticket <- tools:parse_lines(NerbyTickets)]
    }.

process_field(Field) ->
    [Name, Data] = string:split(Field, ": "),
    Ranges =
        [
            begin
                [Min, Max] = string:split(Range, "-"),
                {list_to_integer(Min), list_to_integer(Max)}
            end
         || Range <- string:split(Data, " or ", all)
        ],
    {Name, Ranges}.

verify_any(_Number, []) ->
    false;
verify_any(Number, [{Min, Max} | _Ranges]) when Number >= Min, Number =< Max ->
    true;
verify_any(Number, [_MinMax | Ranges]) ->
    verify_any(Number, Ranges).

verify_all(Values, Ranges) ->
    lists:all(fun(Number) -> verify_any(Number, Ranges) end, Values).

get_values(N, Tickets) ->
    {N, [lists:nth(N, Ticket) || Ticket <- Tickets]}.

find_positions(List, {Name, Ranges}) ->
    {[N || {N, Values} <- List, verify_all(Values, Ranges)], Name}.

get_order([], [], Result) ->
    Result;
get_order([{[Position], Name} | Rest], NoMatch, Result) ->
    Left = [{Positions -- [Position], N} || {Positions, N} <- NoMatch ++ Rest],
    get_order(Left, [], [{Position, Name} | Result]);
get_order([First | Rest], NoMatch, Result) ->
    get_order(Rest, [First | NoMatch], Result).
