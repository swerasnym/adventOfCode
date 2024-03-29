-module(aoc2019_day14).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 14},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    G = build_graph(Data),

    Order = lists:droplast(digraph_utils:topsort(G)),

    F = fun(Result) -> react(Result, Data) end,
    Reactions = lists:map(F, Order),
    Total = lists:foldl(fun do_react/2, #{"FUEL" => 1}, Reactions),
    maps:get("ORE", Total).

star2(Data) ->
    G = build_graph(Data),

    Order = lists:droplast(digraph_utils:topsort(G)),

    F = fun(Result) -> react(Result, Data) end,
    Reactions = lists:map(F, Order),

    Produce =
        fun(Fuel) -> maps:get("ORE", lists:foldl(fun do_react/2, #{"FUEL" => Fuel}, Reactions)) end,

    Start = 1000000000000 div Produce(1),
    search(Start, 1000000000000, Produce).

search(Start, Start, _) ->
    Start;
search(Start, End, F) ->
    Middle = (Start + End + 1) div 2,
    case F(Middle) of
        Value when Value > 1000000000000 ->
            search(Start, Middle - 1, F);
        _ ->
            search(Middle, End, F)
    end.

read(File) ->
    {ok, Device} = file:open(File, [read]),
    read(Device, []).

read(Device, Acc) ->
    case file:read_line(Device) of
        eof ->
            lists:sort(Acc);
        {ok, Line0} ->
            Line = string:trim(Line0),
            [Inputs, Output] = string:split(Line, " => "),
            InputsList = string:split(Inputs, ", ", all),
            F = fun(Elem) ->
                [Is, Element] = string:split(Elem, " "),
                {Element, list_to_integer(Is)}
            end,

            read(Device, [{F(Output), lists:map(F, InputsList)} | Acc])
    end.

build_graph(Data) ->
    G = digraph:new(),
    [build_graph(G, Elem) || Elem <- Data],
    G.

build_graph(G, {{Head, _}, List}) ->
    digraph:add_vertex(G, Head),
    [
        begin
            digraph:add_vertex(G, Elem),
            digraph:add_edge(G, Head, Elem)
        end
     || {Elem, _} <- List
    ].

react(Result, Data) ->
    F = fun({{R, _}, _}) -> R == Result end,

    [React] = lists:filter(F, Data),
    React.

do_react({{Reaction, Output}, List}, Amounts) ->
    Amount = maps:get(Reaction, Amounts, 0),
    No = divup(Amount, Output),
    F = fun({E, A}, Acc) ->
        C = maps:get(E, Acc, 0),
        Acc#{E => C + A * No}
    end,
    lists:foldl(F, Amounts, List).

divup(A, B) ->
    case A rem B of
        0 ->
            A div B;
        _ ->
            1 + A div B
    end.
