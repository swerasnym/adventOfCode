-module(aoc2019_day18).
-behaviour(aoc_solution).

-export([memory/1]).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 18},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Maze, Start, Keys}) ->
    Paths = lists:flatten([reduce(lists:sort(search(Pos, Maze))) || Pos <- Keys]),
    AllKeys = lists:sort([K || {key, K} <- [maps:get(Pos, Maze) || Pos <- Keys]]),
    Connections = maps:from_list(Paths),

    %% Just verify that there is a single path trough doors to get between keys.
    true = length(maps:keys(Connections)) == length(Paths),

    %% Start persistent memory
    Memory = spawn_link(?MODULE, memory, [#{}]),

    {Result, [_Start | Path]} = bfs(Memory, Start, Connections, AllKeys),

    %% Dump memory
    Memory ! {halt, self()},
    receive
        Map ->
            Map
    end,
    io:format("@~s~n", [Path]),
    %% Check how many positions where stored
    Result.

star2({Maze0, {X, Y}, Keys}) ->
    %% TODO make it pass day18_4.data, with 72 where the assumption that the
    %% robots can get all keys in their areas in any order no longer
    %% holds (assuming the required keys age given from the other robots in time.)
    Maze =
        Maze0#{
            {X - 1, Y - 1} => start,
            {X, Y - 1} => wall,
            {X + 1, Y - 1} => start,
            {X - 1, Y} => wall,
            {X, Y} => wall,
            {X + 1, Y} => wall,
            {X - 1, Y + 1} => start,
            {X, Y + 1} => wall,
            {X + 1, Y + 1} => start
        },

    Paths = lists:flatten([reduce(lists:sort(search(Pos, Maze))) || Pos <- Keys]),

    AllKeys = lists:sort([K || {key, K} <- [maps:get(Pos, Maze) || Pos <- Keys]]),

    Starts =
        lists:filter(
            fun
                ({{{start, _}, _}, _}) ->
                    true;
                (_) ->
                    false
            end,
            Paths
        ),

    StartList = [{Pos, To} || {{{start, Pos}, To}, _} <- lists:sort(Starts)],

    Connections = maps:from_list(Paths),
    true = length(maps:keys(Connections)) == length(Paths),

    Memory = spawn_link(?MODULE, memory, [#{}]),

    Results =
        [
            bfs2(Memory, Start, Connections, AllKeys, StartList)
         || Start <- [{X - 1, Y - 1}, {X + 1, Y - 1}, {X - 1, Y + 1}, {X + 1, Y + 1}]
        ],

    Result = [Dist || {Dist, _} <- Results],

    SolutionPath = [["@", Path, " "] || {_, [_Start | Path]} <- Results],

    Memory ! {halt, self()},

    receive
        Map ->
            Map
    end,
    io:format("~s~n", [SolutionPath]),
    lists:sum(Result).

memory(Map) ->
    receive
        {query, Pid, Key} ->
            Pid ! {result, maps:get(Key, Map, new)},
            memory(Map);
        {store, Key, Value} ->
            memory(Map#{Key => Value});
        {halt, Pid} ->
            Pid ! Map
    end.

query(Memory, Key) ->
    Memory ! {query, self(), Key},
    receive
        {result, Value} ->
            {ok, Value}
    after 1000 ->
        error
    end.

store(Memory, Key, Value) ->
    Memory ! {store, Key, Value}.

bfs2(Memory, Pos, Connections, KeysINeed, StartList) ->
    Available = proplists:get_all_values(Pos, StartList),
    bfs(
        Memory,
        {start, Pos},
        sets:from_list(KeysINeed -- Available, [{version, 2}]),
        Available,
        Connections
    ).

bfs(Memory, Pos, Connections, KeysINeed) ->
    bfs(Memory, {start, Pos}, sets:new([{version, 2}]), KeysINeed, Connections).

bfs(_Memory, From, _KeysIHave, [], _Connections) ->
    {0, [From]};
bfs(Memory, From, KeysIHave, KeysINeed, Connections) ->
    case query(Memory, {From, KeysIHave}) of
        {ok, new} ->
            Next = [To || To <- KeysINeed, have_keys(From, To, KeysIHave, Connections)],

            {Distance, Path} =
                lists:min([
                    add(
                        distance(From, N, Connections),
                        bfs(
                            Memory,
                            N,
                            sets:add_element(N, KeysIHave),
                            KeysINeed -- [N],
                            Connections
                        )
                    )
                 || N <- Next
                ]),

            Result = {Distance, [From | Path]},
            store(Memory, {From, KeysIHave}, Result),
            Result;
        {ok, Value} ->
            Value
    end.

add(Dist1, {Dist2, Path}) ->
    {Dist1 + Dist2, Path}.

have_keys(From, To, Keys, Connections) ->
    {Doors, _, _} = maps:get({From, To}, Connections),
    sets:is_subset(Doors, Keys).

distance(From, To, Connections) ->
    {_, Distance, _} = maps:get({From, To}, Connections),
    Distance.

read(File) ->
    {ok, Bin} = file:read_file(File),
    string:split(string:trim(Bin), <<"\n">>, all),
    List = binary_to_list(string:trim(Bin)),
    read(List, 0, 0, #{}, none, []).

read([], _X, _Y, Acc, Start, Keys) ->
    {Acc, Start, Keys};
read([$\n | Rest], _X, Y, Acc, Start, Keys) ->
    read(Rest, 0, Y + 1, Acc, Start, Keys);
read([Char | Rest], X, Y, Acc, Start, Keys) ->
    case Char of
        $# ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => wall}, Start, Keys);
        $@ ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => start}, {X, Y}, Keys);
        $. ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => open}, Start, Keys);
        Key when $a =< Key andalso $z >= Key ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => {key, Key}}, Start, [{X, Y} | Keys]);
        Door when $A =< Door andalso $Z >= Door ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => {door, Door - $A + $a}}, Start, Keys)
    end.

-record(state, {pos, doors = sets:new(), path = [], maze}).

search(Pos, Maze) ->
    {key, To} = maps:get(Pos, Maze),
    search(
        [
            #state{
                pos = Pos,
                maze = Maze,
                path = [to]
            }
        ],
        To,
        Maze,
        []
    ).

search([], _To, _Original, Result) ->
    Result;
search(
    [
        #state{
            pos = Pos,
            doors = Doors,
            path = Path
        } =
            State
        | Rest
    ],
    To,
    Original,
    Result
) ->
    case maps:get(Pos, Original) of
        {door, Door} ->
            State1 = State#state{doors = sets:add_element(Door, Doors)},
            search(Rest ++ next(State1), To, Original, Result);
        {key, Key} ->
            search(
                Rest ++ next(State),
                To,
                Original,
                [{{Key, To}, {Doors, length(Path) - 1, Path}} | Result]
            );
        start ->
            search(
                Rest ++ next(State),
                To,
                Original,
                [{{{start, Pos}, To}, {Doors, length(Path) - 1, Path}} | Result]
            );
        open ->
            search(Rest ++ next(State), To, Original, Result)
    end.

next(
    #state{
        pos = Pos,
        path = Path,
        maze = Maze
    } =
        State
) ->
    NPos =
        lists:filter(
            fun(Neighbour) ->
                case maps:get(Neighbour, Maze) of
                    wall ->
                        false;
                    _ ->
                        true
                end
            end,
            neighbours(Pos)
        ),

    [
        State#state{
            pos = N,
            path = [N | Path],
            maze = Maze#{Pos => wall}
        }
     || N <- NPos
    ].

reduce([{{From, From}, _} | As]) ->
    reduce(As);
reduce([A | As]) ->
    reduce(As, [A]).

reduce([], Result) ->
    Result;
reduce([{{From, From}, {_, _, _}} | Rest], Result) ->
    reduce(Rest, Result);
reduce(
    [{{From, To}, {Doors, Length1, _}} = A | Rest],
    [{{From, To}, {Doors, Length2, _}} | Result]
) when
    Length1 < Length2
->
    reduce(Rest, [A | Result]);
reduce(
    [{{From, To}, {Doors, _Length1, _}} | Rest],
    [{{From, To}, {Doors, _Length2, _}} | _] = Result
) ->
    reduce(Rest, Result);
reduce([A | Rest], Result) ->
    reduce(Rest, [A | Result]).

neighbours({X, Y}) ->
    [{X, Y - 1}, {X, Y + 1}, {X + 1, Y}, {X - 1, Y}].
