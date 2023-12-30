-module(aoc2018_day12).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day12_ex.txt", star1, 325}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 12},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({InitialState, Rules}) ->
    erlang:erase(),
    EndState = next_generation(Rules, InitialState, 20),
    lists:sum(maps:keys(EndState)).

star2({InitialState, Rules}) ->
    erlang:erase(),
    EndState = next_generation(Rules, InitialState, 50000000000),
    lists:sum(maps:keys(EndState)).

read(File) ->
    [InitialState0, Rules0] = tools:read_blocks(File),
    [InitialState1] = tools:parse_format(InitialState0, "initial state: ~s"),
    InitialState = maps:from_list(lists:enumerate(0, InitialState1)),
    Rules1 = tools:parse_lines(Rules0, {fun tools:parse_format/2, ["~s => ~c"]}),
    Rules = #{R => S || [R, [S]] <- Rules1},
    % Sanity check that the vast empty space can't grow anything...
    $. = maps:get(".....", Rules, $.),
    {InitialState, Rules}.

next_generation(_Rules, State, 0) ->
    % Keys = maps:keys(State),
    % Min = lists:min(Keys),
    %  Max = lists:max(Keys),
    % String = state_to_string(Min, Max, State),
    % io:format("~12w ~12w: ~s~n", [0, Min, String]),
    State;
next_generation(Rules, State, Togo) ->
    Keys = maps:keys(State),
    Min = lists:min(Keys),
    Max = lists:max(Keys),
    String = state_to_string(Min, Max, State),
    % io:format("~12w ~12w: ~s~n", [Togo, Min, String]),
    case erlang:get(String) of
        undefined ->
            erlang:put(String, {Togo, Min}),
            NewState =
                #{
                    Pos => $#
                 || Pos <- lists:seq(Min - 2, Max + 2),
                    maps:get(neighbours(Pos, State), Rules, $.) == $#
                },

            next_generation(Rules, NewState, Togo - 1);
        {OldTogo, OldMin} ->
            %% Erase to be able to keep iterating...
            erlang:erase(),
            DTogo = OldTogo - Togo,
            DMin = Min - OldMin,
            Iterations = Togo div DTogo,
            Shift = DMin * Iterations,
            NewState = #{K + Shift => V || K := V <- State},
            next_generation(Rules, NewState, Togo - Iterations)
    end.

neighbours(Pos, Map) ->
    [maps:get(P, Map, $.) || P <- lists:seq(Pos - 2, Pos + 2)].

state_to_string(Min, Max, State) ->
    [maps:get(P, State, $.) || P <- lists:seq(Min, Max)].
