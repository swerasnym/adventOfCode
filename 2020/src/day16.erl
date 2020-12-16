-module(day16).

-export([run/2]).

run(Star, File) ->
    Data = read(File),

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

star1({Fileds, _MyTicket, Tickets}) ->
    {_Lables, Ranges} = lists:unzip(Fileds),
    RangeList = lists:flatten(Ranges),
    {_Ok, Nok} =
        lists:partition(fun(Number) -> verify_any(Number, RangeList) end, lists:flatten(Tickets)),
    lists:sum(Nok).

star2({Fileds, MyTicket, Tickets}) ->
    {_Lables, Ranges} = lists:unzip(Fileds),
    RangeList = lists:flatten(Ranges),
    {Valid, _Invalid} =
        lists:partition(fun(Ticket) -> verify_ticket(Ticket, RangeList) end, Tickets),

    Values = [get_values(N, Valid) || N <- lists:seq(1, length(MyTicket))],
    PossibleMatches = lists:sort([find_pos(Values, Range, []) || Range <- Fileds]),
    Order = get_order(PossibleMatches, [], []),
    Idxs = find_departure_indexes(Order, []),
    product([lists:nth(N, MyTicket) || N <- Idxs]).

product(List) ->
    product(List, 1).

product([A], Result) ->
    A * Result;
product([A | Rest], Result) ->
    product(Rest, Result * A).

find_departure_indexes([], Result) ->
    Result;
find_departure_indexes([{N, {"departure" ++ _, _}} | Rest], Results) ->
    find_departure_indexes(Rest, [N | Results]);
find_departure_indexes([_ | Rest], Results) ->
    find_departure_indexes(Rest, Results).

get_order([], _, Result) ->
    Result;
get_order([{Matches, Range} | PossibleMatches], UsedNumbers, Result) ->
    [N] = lists:foldr(fun(Used, Left) -> lists:delete(Used, Left) end, Matches, UsedNumbers),
    get_order(PossibleMatches, [N | UsedNumbers], [{N, Range} | Result]).

read(File) ->
    {ok, Bin} = file:read_file(File),

    [Fields, MyTicket, NerbyTickets] =
        string:split(
            string:trim(binary_to_list(Bin)), "\n\n", all),

    {process_field(Fields), process_ticket(MyTicket), process_ticket(NerbyTickets)}.

process_field(Fields) ->
    F = fun(Field) ->
           [Name, Data] = string:split(Field, ": "),
           Ranges =
               [begin
                    [Min, Max] = string:split(Range, "-"),
                    {list_to_integer(Min), list_to_integer(Max)}
                end
                || Range <- string:split(Data, " or ", all)],
           {Name, Ranges}
        end,

    [F(Filed) || Filed <- string:split(Fields, "\n", all)].

process_ticket("nearby tickets:\n" ++ NerbyTickets) ->
    [process_ticket(Ticket) || Ticket <- string:split(NerbyTickets, "\n", all)];
process_ticket("your ticket:\n" ++ MyTicket) ->
    process_ticket(MyTicket);
process_ticket(Ticket) ->
    [list_to_integer(Number) || Number <- string:split(Ticket, ",", all)].

verify_any(_Number, []) ->
    false;
verify_any(Number, [{Min, Max} | _Ranges]) when Number >= Min, Number =< Max ->
    true;
verify_any(Number, [_MinMax | Ranges]) ->
    verify_any(Number, Ranges).

verify_ticket(Ticket, Ranges) ->
    case lists:partition(fun(Number) -> verify_any(Number, Ranges) end, Ticket) of
        {_, []} ->
            true;
        _ ->
            false
    end.

find_pos([], Range, Acc) ->
    {Acc, Range};
find_pos([{N, Values} | Rest], {_Name, R} = Range, Acc) ->
    case lists:partition(fun(Number) -> verify_any(Number, R) end, Values) of
        {_, []} ->
            find_pos(Rest, Range, [N | Acc]);
        _ ->
            find_pos(Rest, Range, Acc)
    end.

get_values(N, Tickets) ->
    {N, [lists:nth(N, Ticket) || Ticket <- Tickets]}.
