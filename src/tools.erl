%% @doc tools used for solving AdventOfCode
-module(tools).

-export([
    ws/0,
    count/1, count/2,
    product/1,
    dsort/1,
    rotate/2,
    rotatewhile/2,
    repeat/3,
    group/2,
    replace/2, replace/3, replace/4
]).
-export([read_string/1, read_tokens/2]).
-export([
    read_format/2,
    read_integers/1, read_integers/2, read_integers/3,
    read_lines/1,
    read_lines/2,
    read_blocks/1, read_blocks/2
]).
-export([
    parse_format/2,
    parse_lines/1, parse_lines/2,
    parse_integers/1, parse_integers/2, parse_integers/3,
    parse_blocks/1, parse_blocks/2
]).
-export([as_term/1, eval/1]).
-export([
    read_grid/1, read_grid/2,
    parse_grid/1, parse_grid/2,
    rotate_grid/1,
    rotate_grid/2,
    flip_grid/1, flip_grid/2,
    print_grid/1,
    sub_grid/3,
    drop_max/1,
    lists_to_grid/1,
    grid_to_lists/1, grid_to_lists/2,
    translate_grid/2,
    minmax_grid/1
]).
-export([
    sign/1,
    gcd/1, gcd/2,
    lcm/1, lcm/2,
    egcd/2,
    pow/2,
    mod_inv/2,
    mod/2,
    chinese_remainder/1
]).
-export([interval_from_length/2]).
-export([intervals_overlapp/2]).
-export([interval_sort/1]).
-export([interval_split/2]).
-export([interval_inside/2]).
-export([interval_after/2]).
-export([interval_before/2]).
-export([interval_shift/2]).
-export([overlap/1]).
-export([overlap/2]).
-export([chinese_multi_reminder/1]).

-spec ws() -> string().
ws() ->
    " \t\n\r\v".

%% @doc Generates a map of counnts of the terims in the collection.

-spec count
    (Map) -> #{Value => integer()} when Map :: #{_ => Value};
    (List) -> #{Value => integer()} when List :: [Value].
count(Map) when is_map(Map) ->
    count(maps:values(Map));
count(List) when is_list(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).

%% @doc Counts the number of occurances of 'Value' in the Collection.
-spec count(Value, [Value | any()] | map()) -> integer().
count(Value, List) when is_list(List) ->
    count(Value, List, 0);
count(Value, Map) when is_map(Map) ->
    count(Value, maps:values(Map), 0).

count(_, [], Count) ->
    Count;
count(V, [V | Rest], Count) ->
    count(V, Rest, Count + 1);
count(V, List, Count) ->
    count(V, tl(List), Count).

%% @doc Calculates the procuct of a list of numbers.

-spec product([number()]) -> number().
product(List) ->
    lists:foldl(fun(Term, Product) -> Term * Product end, 1, List).

%% @doc Sorts in decending (reverse) order
-spec dsort([any()]) -> [any()].
dsort(List) ->
    lists:sort(fun erlang:'>'/2, List).

rotate(N, List) ->
    {H, T} = lists:split(mod(N, length(List)), List),
    T ++ H.

rotatewhile(Pred, List) ->
    {H, T} = lists:splitwith(Pred, List),
    T ++ H.

repeat(N, Fun, Acc0) when N >= 1, is_function(Fun, 1) ->
    lists:foldl(fun(_, Acc) -> Fun(Acc) end, Acc0, lists:seq(1, N));
repeat(N, Fun, Acc0) when N >= 1, is_function(Fun, 2) ->
    lists:foldl(Fun, Acc0, lists:seq(1, N)).

group(N, List) when length(List) rem N == 0 ->
    group(N, List, []).

%% internal
group(_, [], Acc) ->
    lists:reverse(Acc);
group(N, List, Acc) ->
    {G, Rest} = lists:split(N, List),
    group(N, Rest, [list_to_tuple(G) | Acc]).

replace(Map, Fun) when is_map(Map), is_function(Fun, 1) ->
    maps:map(fun(_, V) -> Fun(V) end, Map);
replace(Map, Fun) when is_map(Map), is_function(Fun, 2) ->
    maps:map(Fun, Map);
replace(Values, Replacements) when is_map(Replacements) ->
    replace(Values, Replacements, all).

%% @doc All for list
-spec replace(maybe_improper_list() | map(), _, _) -> any().
replace(List, Replacements, all) when is_list(List), is_map(Replacements) ->
    lists:map(fun(Value) -> maps:get(Value, Replacements, Value) end, List);
%% @doc All for map
replace(Map, Replacements, all) when is_map(Map), is_map(Replacements) ->
    maps:map(fun(_Key, Value) -> maps:get(Value, Replacements, Value) end, Map);
%% N for list
replace(List, Replacements, N) when is_list(List), is_map(Replacements), is_integer(N) ->
    replace_n(List, Replacements, N, []);
%% No count at end, replace all...
replace(Values, Replace, With) ->
    replace(Values, #{Replace => With}, all).

%% @doc Relace up to Count occurances off Replace with With in a list
replace(Values, Replace, With, Count) ->
    replace(Values, #{Replace => With}, Count).

%% internal
replace_n([], _Replacements, _N, Acc) ->
    lists:reverse(Acc);
replace_n(List, Replacements, N, []) when N < 0 ->
    lists:reverse(replace_n(lists:reverse(List), Replacements, -N, []));
replace_n(List, _Replacements, 0, Acc) ->
    lists:reverse(Acc, List);
replace_n([Head | Rest], Replacements, N, Acc) when is_map_key(Head, Replacements) ->
    replace_n(Rest, Replacements, N - 1, [maps:get(Head, Replacements) | Acc]);
replace_n([Head | Rest], Replacements, N, Acc) ->
    replace_n(Rest, Replacements, N, [Head | Acc]).

%% ------------------------
%% read

%% @doc Reads a whole file into a string, wihout any trailing whitespace.
read_string(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            string:trim(binary_to_list(Bin), trailing);
        Error ->
            erlang:error(Error, [File])
    end.

%% @doc Tokenizes a whole file using string:tokens/2 ignoring any trailing whitespaces.
read_tokens(File, Separators) ->
    string:tokens(read_string(File), Separators).

%% @doc Reads a whole file into a list of lines without trailing linebreaks.
read_lines(File) ->
    string:split(read_string(File), "\n", all).

%% @doc Reads a whole file into a list of lines without trailing linebreaks and applies function
read_lines(File, Fun) when is_function(Fun, 1) ->
    [Fun(Line) || Line <- read_lines(File)];
read_lines(File, Fun) when is_atom(Fun) ->
    read_lines(File, fun ?MODULE:Fun/1).

%% @doc Reads a whole file into a lists of blocks that where separated by a
%% single empty line.
read_blocks(File) ->
    string:split(read_string(File), "\n\n", all).

%% @doc Reads a whole file into a lists of blocks that where separated by a
%% single empty line and applies function.
read_blocks(File, Fun) when is_function(Fun, 1) ->
    [Fun(Line) || Line <- read_blocks(File)];
read_blocks(File, Fun) when is_atom(Fun) ->
    read_blocks(File, fun ?MODULE:Fun/1).

%% @doc Reads a file of whitespace separated integers to a list.
read_integers(File) ->
    read_integers(File, "\n\r\t\v ").

%% @doc Reads a file of whitespace separated integers to a list and sort them.
read_integers(File, sort) ->
    lists:sort(read_integers(File));
read_integers(File, Separators) ->
    [list_to_integer(Int) || Int <- read_tokens(File, Separators)].

read_integers(File, Separators, sort) ->
    lists:sort(read_integers(File, Separators)).

%% @doc Reads a file of repeated formats ino a list of lits of terms.
%% @see io:format().
%% for format specification
read_format(File, Format) ->
    {ok, Device} = file:open(File, [read]),
    read_format(Device, Format, []).

%% internal implementation
read_format(Device, Format, Acc) ->
    case io:fread(Device, [], Format) of
        eof ->
            ok = file:close(Device),
            lists:reverse(Acc);
        {ok, Terms} ->
            read_format(Device, Format, [Terms | Acc]);
        {error, What} ->
            error({What, Format, Acc})
    end.

%% ------------------------
%% parse

parse_lines(String) ->
    string:split(String, "\n", all).

parse_lines(String, Fun) when is_function(Fun, 1) ->
    [Fun(Line) || Line <- parse_lines(String)];
parse_lines(String, Fun) when is_atom(Fun) ->
    parse_lines(String, fun ?MODULE:Fun/1).

parse_blocks(String) ->
    string:split(String, "\n\n", all).

parse_blocks(String, Fun) when is_function(Fun, 1) ->
    [Fun(Line) || Line <- parse_blocks(String)];
parse_blocks(String, Fun) when is_atom(Fun) ->
    parse_blocks(String, fun ?MODULE:Fun/1).

%% @doc Reads a string of whitespace separated integers to a list.
parse_integers(String) ->
    parse_integers(String, ws()),
    lists:flatten(parse_format(String, "~d")).

%% @doc Reads a file of whitespace separated integers to a list and sort them.
parse_integers(String, sort) ->
    lists:sort(parse_integers(String));
parse_integers(String, Separators) ->
    [list_to_integer(Int) || Int <- string:tokens(String, Separators)].

parse_integers(String, Separators, sort) ->
    lists:sort(parse_integers(String, Separators)).

parse_format(String, Format) ->
    parse_format(String, Format, []).

%% internal implementation
parse_format([], _Format, Acc) ->
    lists:reverse(Acc);
parse_format(String, Format, Acc) ->
    case io_lib:fread(Format, String) of
        {ok, Terms, Rest} ->
            parse_format(Rest, Format, [Terms | Acc]);
        {more, RestFormat, _Nchars, InputStack} ->
            error({partial_match, RestFormat, [InputStack | Acc]});
        {error, What} ->
            error({What, Format, String, Acc})
    end.

%% ------------------------
%% grid

ensure_period_at_end(String) ->
    case lists:last(String) of
        $. ->
            String;
        _ ->
            String ++ [$.]
    end.

as_term(String) ->
    {ok, Tokens, _} = erl_scan:string(ensure_period_at_end(String)),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

eval(String) ->
    {ok, Tokens, _} = erl_scan:string(ensure_period_at_end(String)),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    Result.

drop_max(Grid) ->
    maps:without([max], Grid).

read_grid(File) ->
    parse_grid(read_string(File), {0, 0}, #{}, none).

read_grid(File, Fun) ->
    parse_grid(read_string(File), {0, 0}, #{}, Fun).

parse_grid(String) ->
    parse_grid(String, {0, 0}, #{}, none).

parse_grid(String, Fun) ->
    parse_grid(String, {0, 0}, #{}, Fun).

parse_grid([], _Pos, Grid, _Fun) ->
    Grid#{max => max_grid(Grid)};
parse_grid([$\n], _Pos, Grid, _Fun) ->
    Grid#{max => max_grid(Grid)};
parse_grid([$\n | Rest], {_X, Y}, Grid, Fun) ->
    parse_grid(Rest, {0, Y + 1}, Grid, Fun);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, none) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => Char}, none);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, Map) when is_map(Map) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => maps:get(Char, Map)}, Map);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, Fun) when is_function(Fun, 1) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => Fun(Char)}, Fun);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, Fun) when is_function(Fun, 2) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => Fun(Pos, Char)}, Fun).

lists_to_grid(NestledList) ->
    lists_to_grid({0, 0}, NestledList, #{}).

lists_to_grid({X, Y} = Pos, [[Front | RowRest] | Rest], Grid) ->
    lists_to_grid({X + 1, Y}, [RowRest | Rest], Grid#{Pos => Front});
lists_to_grid({_X, Y}, [[] | Rest], Grid) ->
    lists_to_grid({0, Y + 1}, Rest, Grid);
lists_to_grid(_, [], Grid) ->
    Grid#{max => max_grid(Grid)}.

grid_to_lists(Grid) ->
    grid_to_lists(Grid, missing).

grid_to_lists(#{max := {Xmax, Ymax}} = Grid, Missing) ->
    [[maps:get({X, Y}, Grid, Missing) || X <- lists:seq(0, Xmax)] || Y <- lists:seq(0, Ymax)];
grid_to_lists(Map, Missing) ->
    grid_to_lists(grid_from_2d(Map), Missing).

rotate_grid(Grid) ->
    rotate_grid(Grid, ccw).

rotate_grid(#{max := {Xmax, Ymax}} = Grid, ccw) ->
    NewGrid = #{{Y, Xmax - X} => Value || {X, Y} := Value <- Grid},
    NewGrid#{max => {Ymax, Xmax}};
rotate_grid(#{max := {Xmax, Ymax}} = Grid, cw) ->
    NewGrid = #{{Ymax - Y, X} => Value || {X, Y} := Value <- Grid},
    NewGrid#{max => {Ymax, Xmax}}.

flip_grid(Grid) ->
    flip_grid(Grid, x).

flip_grid(#{max := {Xmax, Ymax}} = Grid, x) ->
    NewGrid =
        maps:from_list([{{Xmax - X, Y}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]),
    NewGrid#{max => {Xmax, Ymax}};
flip_grid(#{max := {Xmax, Ymax}} = Grid, y) ->
    NewGrid =
        maps:from_list([{{X, Ymax - Y}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]),
    NewGrid#{max => {Xmax, Ymax}}.

print_grid(Grid) ->
    io:format("~ts~n", [grid_to_string(Grid)]).

grid_to_string(#{max := {Xmax, Ymax}} = Grid) ->
    string:join(
        [
            [maps:get({X, Y}, Grid, $\s) || X <- lists:seq(0, Xmax)]
         || Y <- lists:seq(0, Ymax)
        ],
        "\n"
    );
grid_to_string(Map) ->
    grid_to_string(grid_from_2d(Map)).

sub_grid(Grid, {Xmin, Ymin}, {Xmax, Ymax}) ->
    SubGrid =
        maps:from_list([
            {{X - Xmin, Y - Ymin}, Value}
         || {{X, Y}, Value} <- maps:to_list(Grid),
            X >= Xmin,
            X =< Xmax,
            Y >= Ymin,
            Y =< Ymax
        ]),
    SubGrid#{max => {Xmax - Xmin, Ymax - Ymin}}.

grid_from_2d(Map) ->
    {{Xmin, Xmax}, {Ymin, Ymax}} = minmax_grid(Map),
    Grid = translate_grid(Map, {-Xmin, -Ymin}),
    Grid#{max => {Xmax - Xmin, Ymax - Ymin}}.

max_grid(#{max := {Xmax, Ymax}}) ->
    {Xmax, Ymax};
max_grid(Grid) ->
    {{_, Xmax}, {_, Ymax}} = minmax_grid(Grid),
    {Xmax, Ymax}.

minmax_grid(Grid) ->
    {Xlist, Ylist} = lists:unzip(maps:keys(maps:without([max], Grid))),
    {{lists:min(Xlist), lists:max(Xlist)}, {lists:min(Ylist), lists:max(Ylist)}}.

translate_grid(#{max := {Mx, My}} = Grid, {Dx, Dy}) ->
    M2 = maps:from_list([{{X + Dx, Y + Dy}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]),
    M2#{max => {Mx + Dx, My + Dy}};
translate_grid(Grid, {Dx, Dy}) ->
    maps:from_list([{{X + Dx, Y + Dy}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]).

%%%%%%%%%%%%%%%%
%% math

sign(0) ->
    0;
sign(N) when is_number(N), N > 0 ->
    1;
sign(N) when is_number(N), N < 0 ->
    -1.

gcd([A]) ->
    A;
gcd([A, B | Rest]) ->
    gcd([gcd(A, B) | Rest]).

gcd(A, 0) ->
    abs(A);
gcd(A, B) ->
    gcd(B, A rem B).

lcm([A]) ->
    A;
lcm([A, B | Rest]) ->
    lcm([lcm(A, B) | Rest]).

lcm(A, B) ->
    A * B div gcd(A, B).

%% @doc egcd(A,B) calculates the tuple {R, X, Y} s.t. A*X + B*Y = R, with R = gcd(A, B).
%% Note that R is allowed to be negative.
egcd(R, 0) ->
    {R, 1, 0};
egcd(A, B) ->
    {R, S, T} = egcd(B, A rem B),
    {R, T, S - A div B * T}.

mod_inv(A, B) ->
    {_R, X, Y} = egcd(A, B),
    case A * X + B * Y =:= 1 of
        true ->
            X;
        false ->
            %% TODO: check what hapens for negative modulii
            %% io:format("~p~n", [_R]),
            undefined
    end.

%% @doc Calculates modulo of two numbers.
%% @return A positive integer
mod(A, M) ->
    X = A rem M,
    case X < 0 of
        true ->
            X + abs(M);
        false ->
            X
    end.

%% @doc Calculates Base^Exponent for integer exponents.
pow(Base, Exponent) -> pow(Base, Exponent, 1).

pow(_, 0, RunningTotal) ->
    RunningTotal;
pow(SquaredBase, Exponent, RunningTotal) when Exponent rem 2 =:= 0 ->
    pow(SquaredBase * SquaredBase, Exponent div 2, RunningTotal);
pow(SquaredBase, Exponent, RunningTotal) ->
    pow(SquaredBase * SquaredBase, Exponent div 2, SquaredBase * RunningTotal).

-spec chinese_remainder(Congruences) -> Result when
    Congruences :: [{Ri, Mi}],
    Result :: {X, M} | undefined,
    Ri :: integer(),
    Mi :: integer(),
    X :: integer(),
    M :: integer().

%% @doc Solves a system of linear congruences:
%% X ~ R1 (mod M1),
%% X ~ R2 (mod M2),
%% ...,
%% X ~ Rn (mod Mn)
%%
%% returns {X, M} such that X + kM is a solution for all integers k; or undefined when no solution exists (i.e. when when gcd(Mi, Mj) does not divide (Ri-Rj) for some i /= j).

chinese_remainder([V]) ->
    V;
chinese_remainder([{R1, M1}, {R2, M2} | Rest]) ->
    {G, P, Q} = egcd(M1, M2),
    case (R1 - R2) rem G of
        0 ->
            M1G = M1 div G,
            M2G = M2 div G,
            %% TODO Remove Sanity check below...
            {1, P, Q} = egcd(M1G, M2G),
            M = M1 * M2G,
            X = mod(R1 * M2G * Q + R2 * M1G * P, M),
            chinese_remainder([{X, M} | Rest]);
        _ ->
            undefined
    end.
-spec chinese_multi_reminder(Congruences) -> Result when
    Congruences :: [{[Ri], Mi}],
    Result :: {[X], M} | undefined,
    Ri :: integer(),
    Mi :: integer(),
    X :: integer(),
    M :: integer().
%% @doc Solves a system of linear congruences with multiple valid resudies:
chinese_multi_reminder([A]) ->
    A;
chinese_multi_reminder([{As, Ma}, {Bs, Mb} | Rest]) ->
    Lcm = lcm(Ma, Mb),
    XsLcm = [R || Ra <- As, Rb <- Bs, (R = chinese_remainder([{Ra, Ma}, {Rb, Mb}])) /= undefined],

    Xs = [X || {X, L} <- XsLcm, L == Lcm],
    %% Sanity check
    true = length(Xs) == length(XsLcm),
    case length(Xs) of
        0 ->
            undefined;
        _ ->
            chinese_multi_reminder([{lists:usort(Xs), Lcm} | Rest])
    end.

%% Intervalls
interval_from_length(Start, 0) ->
    {Start, Start};
interval_from_length(Start, N) when N > 0 ->
    {Start, Start + N};
interval_from_length(Start, N) ->
    {Start + N, Start}.

intervals_overlapp({A1, A2}, {B1, B2}) ->
    A1 =< B2 andalso B1 =< A2;
intervals_overlapp(_, _) ->
    false.

interval_sort({A, B}) when A > B ->
    {B, A};
interval_sort(I) ->
    I.

interval_split({A1, A2} = A, {B1, B2} = B) ->
    case intervals_overlapp(A, B) of
        false ->
            [A];
        true when A1 < B1, A2 > B2 ->
            %% ABBA
            [{A1, B1}, B, {B2, A2}];
        true when A1 < B1 ->
            %% ABAB
            [{A1, B1}, {B1, A2}];
        true when A2 > B2 ->
            %% BABA
            [{A1, B2}, {B2, A2}];
        true ->
            %% BAAB
            [A]
    end;
interval_split(A, _) ->
    A.

%% @doc Returns the part of interval A that exists before B
interval_before({_, A2} = A, {B1, _}) when A2 < B1 ->
    A;
interval_before({A1, _}, {B1, _}) when A1 >= B1 ->
    empty;
interval_before({A1, _}, {B1, _}) ->
    {A1, B1};
interval_before(A, _) ->
    A.

%% @doc Returns the part of interval A that exists after B
interval_after({A1, _} = A, {_, B2}) when A1 > B2 ->
    A;
interval_after({_, A2}, {_, B2}) when A2 =< B2 ->
    empty;
interval_after({_, A2}, {_, B2}) ->
    {B2, A2};
interval_after(A, _) ->
    A.

interval_inside(_, empty) ->
    empty;
interval_inside({A1, A2}, {B1, B2}) when B2 < A1 orelse A2 < B1 ->
    empty;
interval_inside({A1, A2}, {B1, B2} = B) when A1 < B1, B2 < A2 ->
    B;
interval_inside({A1, A2}, {B1, _}) when A1 < B1 ->
    {B1, A2};
interval_inside({A1, A2}, {_, B2}) when B2 < A2 ->
    {A1, B2};
interval_inside(A, _) ->
    A.
interval_shift(empty, _) ->
    empty;
interval_shift({A1, A2}, Amount) ->
    {A1 + Amount, A2 + Amount}.

overlap([]) ->
    [];
overlap([A]) ->
    A;
overlap([A, B | Rest]) ->
    overlap([overlap(A, B, []) | Rest]).

overlap(A, B) ->
    overlap(A, B, []).

overlap(A, B, Overlap) when length(A) == 0 orelse length(B) == 0 ->
    lists:reverse(Overlap);
overlap([A | Ra], [A | Rb], Overlap) ->
    overlap(Ra, Rb, [A | Overlap]);
overlap([A | Rest], B, Overlap) when A < hd(B) ->
    overlap(Rest, B, Overlap);
overlap(A, [B | Rest], Overlap) when B < hd(A) ->
    overlap(A, Rest, Overlap).
