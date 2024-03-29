-module(aoc2021_day16).
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
        problem => {2021, 16},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    L = length(Data) * 4,
    Integer = list_to_integer(Data, 16),
    Message = integer_to_list(Integer, 2),
    Padding = L - length(Message),

    %% {Verssion, Message1} = lists:split(3, Message),
    %% {Type, Message1} = lists:split(3, Message1),
    %% {Value} = lists:split(3, Message),
    {Parsed, _} = parse_message(lists:sublist("0000000", Padding) ++ Message),
    %%  io:format("~p~n~p~n", [Data, Parsed]),
    sum_versions(Parsed).

%% [V1,V2,V3, T1,T2,T3, A1,A2,A3,A4,A5, B1,B2,B3,B4,B5,   ]

%% [[_,_,_] = Head, | [ [_,_,_] = Type |  Rest] ] =
%% {Heder}.
star2(Data) ->
    L = length(Data) * 4,
    Integer = list_to_integer(Data, 16),
    Message = integer_to_list(Integer, 2),
    Padding = L - length(Message),

    %% {Verssion, Message1} = lists:split(3, Message),
    %% {Type, Message1} = lists:split(3, Message1),
    %% {Value} = lists:split(3, Message),
    {Parsed, _} = parse_message(lists:sublist("0000000", Padding) ++ Message),
    %% io:format("~p~n~p~n", [Data, Parsed]),
    compute(Parsed).

read(File) ->
    [Line] = tools:read_lines(File),
    Line.

parse_message(Message) ->
    {Verssion, Message1} = lists:split(3, Message),
    {Type, Message2} = lists:split(3, Message1),
    %%   io:format("~p~n", [{Message, Verssion, Type}]),
    {Data, {rest, Rest}} = parse_message(Type, Message2),

    {{{ver, list_to_integer(Verssion, 2)}, list_to_integer(Type, 2), Data}, {rest, Rest}}.

parse_message("100", Message) ->
    parse_value(Message, []);
parse_message(_, [Lt | Message]) ->
    case Lt of
        $0 ->
            {LengthB, Message1} = lists:split(15, Message),
            %%    io:format("~p~n", [{LengthB, Message1}]),
            {SubPacket, Rest} = lists:split(list_to_integer(LengthB, 2), Message1),
            {sp(SubPacket, []), {rest, Rest}};
        $1 ->
            {PacketsB, Rest} = lists:split(11, Message),
            sub_packages(list_to_integer(PacketsB, 2), Rest, [])
    end.

parse_value(Message, Acc) ->
    {[H | Value], Rest} = lists:split(5, Message),
    case H of
        $1 ->
            parse_value(Rest, Acc ++ Value);
        $0 ->
            {{integer, list_to_integer(Acc ++ Value, 2)}, {rest, Rest}}
    end.

sp([], Acc) ->
    lists:reverse(Acc);
sp(Message, Acc) ->
    {Data, {rest, Rest}} = parse_message(Message),
    sp(Rest, [Data | Acc]).

sub_packages(0, Rest, Acc) ->
    {lists:reverse(Acc), {rest, Rest}};
sub_packages(N, Message, Acc) ->
    {Data, {rest, Rest}} = parse_message(Message),
    sub_packages(N - 1, Rest, [Data | Acc]).

sum_versions({{ver, V}, _T, Data}) ->
    V + sum_versions(Data);
sum_versions([{{ver, V}, _T, Data} | Rest]) ->
    V + sum_versions(Data) + sum_versions(Rest);
sum_versions([]) ->
    0;
sum_versions({integer, D}) when is_integer(D) ->
    0.

compute({_V, 0, Data}) ->
    lists:sum([compute(Sp) || Sp <- Data]);
compute({_V, 1, Data}) ->
    tools:product([compute(Sp) || Sp <- Data]);
compute({_V, 2, Data}) ->
    lists:min([compute(Sp) || Sp <- Data]);
compute({_V, 3, Data}) ->
    lists:max([compute(Sp) || Sp <- Data]);
compute({_V, 4, {integer, D}}) ->
    D;
compute({_V, 5, [Sp1, Sp2]}) ->
    case compute(Sp1) > compute(Sp2) of
        true ->
            1;
        false ->
            0
    end;
compute({_V, 6, [Sp1, Sp2]}) ->
    case compute(Sp1) < compute(Sp2) of
        true ->
            1;
        false ->
            0
    end;
compute({_V, 7, [Sp1, Sp2]}) ->
    case compute(Sp1) == compute(Sp2) of
        true ->
            1;
        false ->
            0
    end.
