-module(aoc2018_day7).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, star2/2, read/1]).
-export([input_to_graphviz/1]).

info() ->
    Examples = [
        {"examples/2018/day7_ex.txt", star1, "CABDFE"},
        {"examples/2018/day7_ex.txt", {star2, {2, 0}}, 15}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 7},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Edges) ->
    G = make_graph(Edges),
    Order = order(G),
    lists:concat(Order).

star2(Edges) ->
    star2(Edges, {5, 60}).
star2(Edges, {Workers, TimeAdded}) ->
    G = make_graph(Edges),
    work(G, Workers, TimeAdded).

read(File) ->
    tools:read_multiple_formats(File, "Step ~c must be finished before step ~c can begin.").

make_graph(Edges) ->
    Vertices = lists:usort(lists:concat(Edges)),
    G = digraph:new(),
    [digraph:add_vertex(G, V) || V <- Vertices],
    [digraph:add_edge(G, V1, V2) || [V1, V2] <- Edges],

    G.

input_to_graphviz(input) ->
    {ok, File} = aoc_web:get_input_path(maps:get(problem, info())),
    input_to_graphviz(File);
input_to_graphviz(File) ->
    Edges = read(File),
    io:format("digraph G{~n"),
    [io:format("~s -> ~s~n", E) || E <- Edges],
    io:format("}~n").

order(G) ->
    Vs = lists:sort([V || V <- digraph:vertices(G), digraph:in_degree(G, V) == 0]),
    order(G, Vs, []).
order(_G, [], Acc) ->
    lists:reverse(Acc);
order(G, [V | Vertices], Acc) ->
    Neighbours = digraph:out_neighbours(G, V),
    digraph:del_vertex(G, V),
    New = lists:sort([N || N <- Neighbours, digraph:in_degree(G, N) == 0]),

    order(G, lists:merge(Vertices, New), [V | Acc]).

time([Task], TimeAdded) ->
    TimeAdded + Task - $A + 1.

work(G, Workers, TimeAdded) ->
    WorkAvailable = lists:sort([V || V <- digraph:vertices(G), digraph:in_degree(G, V) == 0]),

    work(G, Workers, TimeAdded, WorkAvailable, [], 0).

work(_G, _Workers, _TimeAdded, [], [], Time) ->
    %% All work is done, report the time taken!
    Time;
work(G, Workers, TimeAdded, WorkAvailable, [{0, Task} | WorkStarted], Time) ->
    %% A task was finished! Free up the worker and find out if new tasks become available.
    io:format("~4w: Task ~s is done~n", [Time, Task]),
    DependentTasks = digraph:out_neighbours(G, Task),
    digraph:del_vertex(G, Task),
    NewTasks = lists:sort([Dt || Dt <- DependentTasks, digraph:in_degree(G, Dt) == 0]),
    work(G, Workers + 1, TimeAdded, lists:merge(WorkAvailable, NewTasks), WorkStarted, Time);
work(G, 0, TimeAdded, WorkAvailable, WorkStarted, Time) ->
    %% No workers available, let time pass
    {Dt, _} = hd(WorkStarted),
    Work = [{TimeLeft - Dt, Task} || {TimeLeft, Task} <- WorkStarted],
    work(G, 0, TimeAdded, WorkAvailable, Work, Time + Dt);
work(G, Workers, TimeAdded, [], WorkStarted, Time) ->
    %% No tasks available to start, let time pass.
    {Dt, _} = hd(WorkStarted),
    Work = [{TimeLeft - Dt, Task} || {TimeLeft, Task} <- WorkStarted],
    work(G, Workers, TimeAdded, [], Work, Time + Dt);
work(G, Workers, TimeAdded, [Task | WorkAvailable], WorkStarted, Time) ->
    %% Both workers and new tasks available! Start a new task and allocate a worker.
    io:format("~4w: Task ~s was started~n", [Time, Task]),
    NewlyStarted = [{time(Task, TimeAdded), Task}],
    work(G, Workers - 1, TimeAdded, WorkAvailable, lists:merge(WorkStarted, NewlyStarted), Time).
