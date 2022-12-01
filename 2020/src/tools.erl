-module(tools).

-export([ws/0, count/1, count/2, product/1, dsort/1, replace/2, replace/3, replace/4]).
-export([read_string/1, read_tokens/2]).
-export([read_format/2, read_integers/1, read_integers/2, read_integers/3, read_lines/1,
         read_lines/2, read_blocks/1, read_blocks/2]).
-export([parse_format/2, parse_lines/1, parse_lines/2, parse_integers/1, parse_integers/2,
         parse_integers/3, parse_blocks/1, parse_blocks/2]).
-export([as_term/1, eval/1]).
-export([read_grid/1, read_grid/2, parse_grid/1, parse_grid/2, rotate_grid/1,
         rotate_grid/2, flip_grid/1, flip_grid/2, print_grid/1, sub_grid/3, drop_max/1,
         lists_to_grid/1, grid_to_lists/1, grid_to_lists/2, translate_grid/2, minmax_grid/1]).
-export([gcd/2, egcd/2, mod_inv/2, mod/2, chinese_remainder/1]).

ws() ->
    " \t\n\r\v".

%% @doc Generates a map of counnts of the terims in the collection.
count(Map) when is_map(Map) ->
    count(maps:values(Map));
count(List) when is_list(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).

%% @doc Counts the number of occurances of Value in Collection.
count(Value, Collection) ->
    maps:get(Value, count(Collection), 0).

%% @doc Calculates the procuct of a list of numbers.
product(List) ->
    lists:foldl(fun(Term, Product) -> Term * Product end, 1, List).

%% Sort in decending (reverse) order
dsort(List) ->
    lists:sort(fun erlang:'>'/2, List).

replace(Values, Replacements) when is_map(Replacements) ->
    replace(Values, Replacements, all).

%% All for list
replace(List, Replacements, all) when is_list(List), is_map(Replacements) ->
    lists:map(fun(Value) -> maps:get(Value, Replacements, Value) end, List);
%% All for map
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
    {ok, Bin} = file:read_file(File),
    string:trim(binary_to_list(Bin), trailing).

%% @doc Tokenizes a whole file using string:tokens/2 ignoring any trailing whitespaces.
read_tokens(File, Separators) ->
    string:tokens(read_string(File), Separators).

%% @doc Reads a whole file into a list of lines without trailing linebreaks.
read_lines(File) ->
    string:split(read_string(File), "\n", all).

%% @dock Reads a whole file into a list of lines without trailing linebreaks and applies function
read_lines(File, Fun) when is_function(Fun, 1) ->
    [Fun(Line) || Line <- read_lines(File)];
read_lines(File, Fun) when is_atom(Fun) ->
    read_lines(File, fun ?MODULE:Fun/1).

%% @doc Reads a whole file into a lists of blocks that where separated by a
%% single empty line.
read_blocks(File) ->
    string:split(read_string(File), "\n\n", all).

%% @dock Reads a whole file into a lists of blocks that where separated by a
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
%% @see io:fread for format specification.
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

as_term(String) ->
    Str = case lists:last(String) of
              $. ->
                  String;
              _ ->
                  String ++ [$.]
          end,
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Term} = erl_parse:parse_term(Tokens),
    Term.

eval(String) ->
    Str = case lists:last(String) of
              $. ->
                  String;
              _ ->
                  String ++ [$.]
          end,
    {ok, Tokens, _} = erl_scan:string(Str),
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
    {{_Xmin, Xmax}, {_Ymin, Ymax}} = minmax_grid(Grid),
    Grid#{max => {Xmax, Ymax}};
parse_grid([$\n], _Pos, Grid, _Fun) ->
    {{_Xmin, Xmax}, {_Ymin, Ymax}} = minmax_grid(Grid),
    Grid#{max => {Xmax, Ymax}};
parse_grid([$\n | Rest], {_X, Y}, Grid, Fun) ->
    parse_grid(Rest, {0, Y + 1}, Grid, Fun);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, none) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => Char}, none);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, Map) when is_map(Map) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => maps:get(Char, Map)}, Map);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, Fun) when is_function(Fun) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => Fun(Char)}, Fun).

lists_to_grid(NestledList) ->
    lists_to_grid({0, 0}, NestledList, #{}).

lists_to_grid({X, Y} = Pos, [[Front | RowRest] | Rest], Grid) ->
    lists_to_grid({X + 1, Y}, [RowRest | Rest], Grid#{Pos => Front});
lists_to_grid({_X, Y}, [[] | Rest], Grid) ->
    lists_to_grid({0, Y + 1}, Rest, Grid);
lists_to_grid(_, [], Grid) ->
    {{_Xmin, Xmax}, {_Ymin, Ymax}} = minmax_grid(Grid),
    Grid#{max => {Xmax, Ymax}}.

grid_to_lists(Grid) ->
    grid_to_lists(Grid, missing).

grid_to_lists(Grid = #{max := {Xmax, Ymax}}, Missing) ->
    [[maps:get({X, Y}, Grid, Missing) || X <- lists:seq(0, Xmax)] || Y <- lists:seq(0, Ymax)];
grid_to_lists(Map, Missing) ->
    grid_to_lists(grid_from_2d(Map), Missing).

rotate_grid(Grid) ->
    rotate_grid(Grid, ccw).

rotate_grid(Grid = #{max := {Xmax, Ymax}}, ccw) ->
    NewGrid =
        maps:from_list([{{Y, Xmax - X}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]),
    NewGrid#{max => {Ymax, Xmax}};
rotate_grid(Grid = #{max := {Xmax, Ymax}}, cw) ->
    NewGrid =
        maps:from_list([{{Ymax - Y, X}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]),
    NewGrid#{max => {Ymax, Xmax}}.

flip_grid(Grid) ->
    flip_grid(Grid, x).

flip_grid(Grid = #{max := {Xmax, Ymax}}, x) ->
    NewGrid =
        maps:from_list([{{Xmax - X, Y}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]),
    NewGrid#{max => {Xmax, Ymax}};
flip_grid(Grid = #{max := {Xmax, Ymax}}, y) ->
    NewGrid =
        maps:from_list([{{X, Ymax - Y}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]),
    NewGrid#{max => {Xmax, Ymax}}.

print_grid(Grid) ->
    io:format("~ts~n", [grid_to_string(Grid)]).

grid_to_string(Grid = #{max := {Xmax, Ymax}}) ->
    string:join([[maps:get({X, Y}, Grid, $ ) || X <- lists:seq(0, Xmax)]
                 || Y <- lists:seq(0, Ymax)],
                "\n");
grid_to_string(Map) ->
    grid_to_string(grid_from_2d(Map)).

sub_grid(Grid, {Xmin, Ymin}, {Xmax, Ymax}) ->
    SubGrid =
        maps:from_list([{{X - Xmin, Y - Ymin}, Value}
                        || {{X, Y}, Value} <- maps:to_list(Grid),
                           X >= Xmin,
                           X =< Xmax,
                           Y >= Ymin,
                           Y =< Ymax]),
    SubGrid#{max => {Xmax - Xmin, Ymax - Ymin}}.

grid_from_2d(Map) ->
    {{Xmin, Xmax}, {Ymin, Ymax}} = minmax_grid(Map),
    Grid = translate_grid(Map, {-Xmin, -Ymin}),
    Grid#{max => {Xmax - Xmin, Ymax - Ymin}}.

minmax_grid(Grid) ->
    {Xlist, Ylist} =
        lists:unzip(
            maps:keys(
                maps:without([max], Grid))),
    {{lists:min(Xlist), lists:max(Xlist)}, {lists:min(Ylist), lists:max(Ylist)}}.

translate_grid(#{max := {Mx, My}} = Grid, {Dx, Dy}) ->
    M2 = maps:from_list([{{X + Dx, Y + Dy}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]),
    M2#{max => {Mx + Dx, My + Dy}};
translate_grid(Grid, {Dx, Dy}) ->
    maps:from_list([{{X + Dx, Y + Dy}, Value} || {{X, Y}, Value} <- maps:to_list(Grid)]).

%%%%%%%%%%%%%%%%
%% math
gcd(A, 0) ->
    abs(A);
gcd(A, B) ->
    gcd(B, A rem B).

egcd(_, 0) ->
    {1, 0};
egcd(A, B) ->
    {S, T} = egcd(B, A rem B),
    {T, S - A div B * T}.

mod_inv(A, B) ->
    {X, Y} = egcd(A, B),
    if A * X + B * Y =:= 1 ->
           X;
       true ->
           undefined
    end.

mod(A, M) ->
    X = A rem M,
    if X < 0 ->
           X + abs(M);
       true ->
           X
    end.

%% internal
calc_inverses([], []) ->
    [];
calc_inverses([N | Ns], [M | Ms]) ->
    case mod_inv(N, M) of
        undefined ->
            undefined;
        Inv ->
            [Inv | calc_inverses(Ns, Ms)]
    end.

chinese_remainder(Congruences) ->
    {Residues, Modulii} = lists:unzip(Congruences),
    ModPI = lists:foldl(fun(A, B) -> A * B end, 1, Modulii),
    CRT_Modulii = [ModPI div M || M <- Modulii],
    case calc_inverses(CRT_Modulii, Modulii) of
        undefined ->
            undefined;
        Inverses ->
            Solution =
                lists:sum([A * B
                           || {A, B}
                                  <- lists:zip(CRT_Modulii,
                                               [A * B
                                                || {A, B} <- lists:zip(Residues, Inverses)])]),
            mod(Solution, ModPI)
    end.
