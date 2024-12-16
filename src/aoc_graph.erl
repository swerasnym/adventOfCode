-module(aoc_graph).
-export([a_star/4]).
-export([dijkstra/3]).
-export([get_path/2]).
-export([get_multiple_paths/2]).

-export_type([pos/0]).

-type pos() :: term().

-spec a_star(Start | StartList, End, Neighbours, Estimate) ->
    {
        Dist :: non_neg_integer(),
        EndPoint :: End | EndPos :: pos(),
        Visited :: #{
            Start := {0, start}, Pos :: pos() => {Distance :: non_neg_integer(), From :: pos()}
        }
    }
when
    Start :: pos(),
    StartList :: [pos()],
    End :: pos() | fun((Pos :: pos()) -> boolean()),
    Neighbours :: fun((Pos :: pos()) -> [{D :: non_neg_integer(), Neighbour :: pos()}]),
    Estimate :: fun((Pos :: pos()) -> E :: non_neg_integer()).

%% @doc Calculate the shortest distance between a (set of) start and an end (condition)
%% using the a_star algorithm.
%%
%% Note: For the distance to be truly the shortest the Estimate function must be admissible,
%% i.e. never overestimate the distance from the current position to the/an end.
a_star(Start, End, Neighbours, Estimate) when not is_list(Start) ->
    a_star([Start], End, Neighbours, Estimate);
a_star(StartList, End, Neighbours, Estimate) when
    is_list(StartList),
    is_function(Neighbours, 1),
    is_function(Estimate, 1)
->
    Starts = [{0, 0, S, start} || S <- StartList],
    case is_function(End, 1) of
        true ->
            a_star(gb_sets:from_list(Starts), End, #{}, Neighbours, Estimate);
        false ->
            a_star(
                gb_sets:from_list(Starts), fun(Pos) -> Pos == End end, #{}, Neighbours, Estimate
            )
    end.

%% @doc Same as a_star but with an estimate that always is 0.
dijkstra(Start, End, Neighbours) ->
    a_star(Start, End, Neighbours, fun(_) -> 0 end).

get_path(Pos, Visited) when is_map_key(Pos, Visited) ->
    hd(get_multiple_paths(Pos, Visited, [])).

get_multiple_paths(Pos, Visited) when is_map_key(Pos, Visited) ->
    get_multiple_paths(Pos, Visited, []).

%% --- Internal implementations ---
a_star({0, nil}, _End, Visited, _Neighbours, _Estimate) ->
    {no_path, Visited};
a_star(States, End, Visited, Neighbours, Estimate) ->
    {State, Rest} = gb_sets:take_smallest(States),
    {_Est, Dist, Pos, From} = State,
    case {End(Pos), maps:is_key(Pos, Visited)} of
        {true, false} ->
            {Dist, Pos, Visited#{Pos => {Dist, [From]}}};
        {false, false} ->
            New = Neighbours(Pos),
            NewStates = [{Dist + D + Estimate(N), Dist + D, N, Pos} || {D, N} <- New],
            Next = gb_sets:union(Rest, gb_sets:from_list(NewStates)),
            a_star(Next, End, Visited#{Pos => {Dist, [From]}}, Neighbours, Estimate);
        {false, true} ->
            case maps:get(Pos, Visited) of
                {Dist, From2} ->
                    a_star(
                        Rest, End, Visited#{Pos => {Dist, [From | From2]}}, Neighbours, Estimate
                    );
                _ ->
                    a_star(Rest, End, Visited, Neighbours, Estimate)
            end
    end.

get_multiple_paths(Pos, Visited, Path) ->
    case maps:get(Pos, Visited) of
        {0, [start]} ->
            [[{0, Pos} | Path]];
        {Dist, [From]} ->
            get_multiple_paths(From, Visited, [{Dist, Pos} | Path]);
        {Dist, Froms} ->
            [Mp ++ Path || From <- Froms, Mp <- get_multiple_paths(From, Visited, [{Dist, Pos}])]
    end.
