-module(day18).

-export([run/2, memmory/1]).

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

star1({Maze, Start, Keys}) ->
    Paths = lists:flatten([reduce(lists:sort(search(Pos, Maze))) || Pos <- Keys]),
    AllKeys = lists:sort([K || {key, K} <- [maps:get(Pos, Maze) || Pos <- Keys]]),
    Connections = maps:from_list(Paths),

    %% Just verify thet there is a single path trough doors to get between keys.
    true = length(maps:keys(Connections)) == length(Paths),

    %% Start persistent memory
    Memmory = spawn_link(day18, memmory, [#{}]),

    {Result, [_Start | Path]} = bfs(Memmory, Start, Connections, AllKeys),

    %% Dump memmory
    Memmory ! {halt, self()},
    receive
        Map ->
            Map
    end,

    %% Check how many possitions where stored
    {Result, [$@ | Path]}.

star2({Maze0, {X, Y}, Keys}) ->
    %% TODO make it pass day18_4.data, with 72 where the assumption that the
    %% robots can get all keys in their areas in any order no longer
    %% holds (assumong the required keys age given from the other robots in time.)
    Maze =
        Maze0#{{X - 1, Y - 1} => start,
               {X, Y - 1} => wall,
               {X + 1, Y - 1} => start,
               {X - 1, Y} => wall,
               {X, Y} => wall,
               {X + 1, Y} => wall,
               {X - 1, Y + 1} => start,
               {X, Y + 1} => wall,
               {X + 1, Y + 1} => start},

    Paths = lists:flatten([reduce(lists:sort(search(Pos, Maze))) || Pos <- Keys]),

    AllKeys = lists:sort([K || {key, K} <- [maps:get(Pos, Maze) || Pos <- Keys]]),

    Starts =
        lists:filter(fun ({{{start, _}, _}, _}) ->
                             true;
                         (_) ->
                             false
                     end,
                     Paths),

    StartList = [{Pos, To} || {{{start, Pos}, To}, _} <- lists:sort(Starts)],

    Connections = maps:from_list(Paths),
    true = length(maps:keys(Connections)) == length(Paths),

    Memmory = spawn_link(day18, memmory, [#{}]),

    Results =
        [bfs2(Memmory, Start, Connections, AllKeys, StartList)
         || Start <- [{X - 1, Y - 1}, {X + 1, Y - 1}, {X - 1, Y + 1}, {X + 1, Y + 1}]],

    Result = [Dist || {Dist, _} <- Results],

    SolutionPath = [["@", Path, " "] || {_, [_Start | Path]} <- Results],

    Memmory ! {halt, self()},

    receive
        Map ->
            Map
    end,
    {lists:sum(Result),
     lists:concat(
         lists:concat(SolutionPath))}.

memmory(Map) ->
    receive
        {query, Pid, Key} ->
            Pid ! {result, maps:get(Key, Map, new)},
            memmory(Map);
        {store, Key, Value} ->
            memmory(Map#{Key => Value});
        {halt, Pid} ->
            Pid ! Map
    end.

query(Memmory, Key) ->
    Memmory ! {query, self(), Key},
    receive
        {result, Value} ->
            {ok, Value}
    after 1000 ->
        error
    end.

store(Memmory, Key, Value) ->
    Memmory ! {store, Key, Value}.

bfs2(Memmory, Pos, Connections, KeysINeed, StartList) ->
    Available = proplists:get_all_values(Pos, StartList),
    bfs(Memmory,
        {start, Pos},
        sets:from_list(KeysINeed -- Available),
        Available,
        Connections).

bfs(Memmory, Pos, Connections, KeysINeed) ->
    bfs(Memmory, {start, Pos}, sets:new(), KeysINeed, Connections).

bfs(_Memmory, From, _KeysIHave, [], _Connections) ->
    {0, [From]};
bfs(Memmory, From, KeysIHave, KeysINeed, Connections) ->
    case query(Memmory, {From, KeysIHave}) of
        {ok, new} ->
            Nexts = [To || To <- KeysINeed, have_keys(From, To, KeysIHave, Connections)],

            {Distance, Path} =
                lists:min([add(distance(From, Next, Connections),
                               bfs(Memmory,
                                   Next,
                                   sets:add_element(Next, KeysIHave),
                                   KeysINeed -- [Next],
                                   Connections))
                           || Next <- Nexts]),

            Result = {Distance, [From | Path]},
            store(Memmory, {From, KeysIHave}, Result),
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
    string:split(
        string:trim(Bin), <<"\n">>, all),
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
    search([#state{pos = Pos,
                   maze = Maze,
                   path = [to]}],
           To,
           Maze,
           []).

search([], _To, _Original, Result) ->
    Result;
search([#state{pos = Pos,
               doors = Doors,
               path = Path} =
            State
        | Rest],
       To,
       Original,
       Result) ->
    case maps:get(Pos, Original) of
        {door, Door} ->
            State1 = State#state{doors = sets:add_element(Door, Doors)},
            search(Rest ++ next(State1), To, Original, Result);
        {key, Key} ->
            search(Rest ++ next(State),
                   To,
                   Original,
                   [{{Key, To}, {Doors, length(Path) - 1, Path}} | Result]);
        start ->
            search(Rest ++ next(State),
                   To,
                   Original,
                   [{{{start, Pos}, To}, {Doors, length(Path) - 1, Path}} | Result]);
        open ->
            search(Rest ++ next(State), To, Original, Result)
    end.

next(#state{pos = Pos,
            path = Path,
            maze = Maze} =
         State) ->
    Npos =
        lists:filter(fun(Neigbour) ->
                        case maps:get(Neigbour, Maze) of
                            wall -> false;
                            _ -> true
                        end
                     end,
                     neigbours(Pos)),

    [State#state{pos = N,
                 path = [N | Path],
                 maze = Maze#{Pos => wall}}
     || N <- Npos].

reduce([{{From, From}, _} | As]) ->
    reduce(As);
reduce([A | As]) ->
    reduce(As, [A]).

reduce([], Result) ->
    Result;
reduce([{{From, From}, {_, _, _}} | Rest], Result) ->
    reduce(Rest, Result);
reduce([{{From, To}, {Doors, Length1, _}} = A | Rest],
       [{{From, To}, {Doors, Length2, _}} | Result])
    when Length1 < Length2 ->
    reduce(Rest, [A | Result]);
reduce([{{From, To}, {Doors, _Length1, _}} | Rest],
       [{{From, To}, {Doors, _Length2, _}} | _] = Result) ->
    reduce(Rest, Result);
reduce([A | Rest], Result) ->
    reduce(Rest, [A | Result]).

neigbours({X, Y}) ->
    [{X, Y - 1}, {X, Y + 1}, {X + 1, Y}, {X - 1, Y}].
