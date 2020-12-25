-module(tools).

-export([count/1, count/2, product/1]).
-export([read_string/1]).
-export([read_format/2, read_integers/1, read_lines/1, read_blocks/1, read_grid/1,
         read_grid/2]).
-export([parse_format/2, parse_lines/1, parse_integers/1, parse_blocks/1, parse_grid/1,
         parse_grid/2]).

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

%% ------------------------
%% read

%% @doc Reads a whole file into a string, wihout any trailing whitespace.
read_string(File) ->
    {ok, Bin} = file:read_file(File),
    string:trim(binary_to_list(Bin), trailing).

%% @doc Reads a whole file into a list of lines without trailing linebreaks.
read_lines(File) ->
    string:split(read_string(File), "\n", all).

%% @doc Reads a whole file into a lists of blocks that where separated by a
%% single empty line.
read_blocks(File) ->
    string:split(read_string(File), "\n\n", all).

%% @doc Reads a file of whitespace separated integers to a list.
read_integers(File) ->
    lists:flatten(read_format(File, "~d")).

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

parse_blocks(String) ->
    string:split(String, "\n\n", all).

%% @doc Reads a string of whitespace separated integers to a list.
parse_integers(String) ->
    lists:flatten(parse_format(String, "~d")).

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

read_grid(File) ->
    parse_grid(read_string(File), {0, 0}, #{}, none).

read_grid(File, Fun) ->
    parse_grid(read_string(File), {0, 0}, #{}, Fun).

parse_grid(String) ->
    parse_grid(String, {0, 0}, #{}, none).

parse_grid(String, Fun) ->
    parse_grid(String, {0, 0}, #{}, Fun).

parse_grid([], {X, Y}, Grid, _Fun) ->
    Grid#{max => {X - 1, Y}};
parse_grid([$\n], {X, Y}, Grid, _Fun) ->
    Grid#{max => {X - 1, Y}};
parse_grid([$\n | Rest], {_X, Y}, Grid, Fun) ->
    parse_grid(Rest, {0, Y + 1}, Grid, Fun);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, none) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => Char}, none);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, Map) when is_map(Map) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => maps:get(Char, Map)}, Map);
parse_grid([Char | Rest], {X, Y} = Pos, Grid, Fun) when is_function(Fun) ->
    parse_grid(Rest, {X + 1, Y}, Grid#{Pos => Fun(Char)}, Fun).
