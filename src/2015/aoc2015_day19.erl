-module(aoc2015_day19).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day19_ex.txt", star1, 7},
        {"examples/2015/day19_ex2.txt", star2, 6}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 19},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Start, Replacements}) ->
    length(step(Start, Replacements)).

star2({S, R}) ->
    AtomReplacements = lists:usort([{F, to_atom(F)} || [F, _] <- R]),
    Replacements = [[to_atom(F), atomize(T, AtomReplacements)] || [F, T] <- R],

    case lists:all(fun([_, T]) -> length(T) > 1 end, Replacements) of
        false ->
            construct(S, R, ["e"], 0);
        true ->
            Start = atomize(S, AtomReplacements),
            Class = [classify(I) || I <- Start],
            tools:count(atom, Class) - tools:count(comma, Class)
    end.

read(File) ->
    [Replacements, Start] = tools:read_blocks(File),
    {Start, tools:parse_multiple_formats(Replacements, "~s => ~s")}.

replace({From, To}, {Prefix, String}, Acc) ->
    case string:split(String, From) of
        [_] ->
            Acc;
        [Head, Tail] ->
            NewPrefix = Prefix ++ Head ++ From,
            NewMolecule = Prefix ++ Head ++ To ++ Tail,
            replace({From, To}, {NewPrefix, Tail}, [NewMolecule | Acc])
    end.

step(Start, Replacements) ->
    lists:usort([I || [From, To] <- Replacements, I <- replace({From, To}, {"", Start}, [])]).

construct(_, _, [], _) ->
    error;
construct(Goal, Replacements, Constructed, Steps) ->
    io:format("Step~p: Molecules ~p~n", [Steps, Constructed]),

    New = [L || C <- Constructed, L <- step(C, Replacements)],
    case lists:member(Goal, New) of
        true ->
            Steps + 1;
        false ->
            construct(Goal, Replacements, lists:usort(New), Steps + 1)
    end.

to_atom(S) ->
    erlang:list_to_atom(string:to_lower(S)).

atomize([], []) ->
    [];
atomize(String, []) ->
    {String};
atomize(String, [{As, A} | Rest]) ->
    Splits = string:split(String, As, all),
    case length(Splits) of
        1 ->
            atomize(String, Rest);
        _ ->
            Recurse = [atomize(S, Rest) || S <- Splits],
            lists:flatten(lists:join(A, Recurse))
    end.

classify(A) when is_atom(A) ->
    atom;
classify({"Y"}) ->
    comma;
classify(O) ->
    O.
