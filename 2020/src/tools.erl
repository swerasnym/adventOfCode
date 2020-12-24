-module(tools).

-export([count/1, count/2]).
-export([read_string/1, read_integers/1, read_lines/1, read_blocks/1, read_grid/1,
         read_grid/2]).
-export([parse_lines/1, parse_integers/1, parse_blocks/1, parse_grid/1, parse_grid/2]).

count(Map) when is_map(Map) ->
    count(maps:values(Map));
count(List) when is_list(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).

count(Value, Collection) ->
    maps:get(Value, count(Collection), 0).

read_integers(File) ->
    parse_integers(read_string(File), []).

parse_integers(String) ->
    parse_integers(String, []).

read_string(File) ->
    {ok, Bin} = file:read_file(File),
    string:trim(binary_to_list(Bin), trailing).

read_lines(File) ->
    string:split(read_string(File), "\n").

parse_lines(String) ->
    string:split(String, "\n").

read_blocks(File) ->
    string:split(read_string(File), "\n\n").

parse_blocks(String) ->
    string:split(String, "\n\n").

read_grid(File) ->
    parse_grid(read_string(File), {0, 0}, #{}, none).

read_grid(File, Fun) ->
    parse_grid(read_string(File), {0, 0}, #{}, Fun).

parse_grid(String) ->
    parse_grid(String, {0, 0}, #{}, none).

parse_grid(String, Fun) ->
    parse_grid(String, {0, 0}, #{}, Fun).

%%% Internal Functions

parse_integers([], Acc) ->
    lists:reverse(Acc);
parse_integers(String, Acc) ->
    case string:to_integer(
             string:trim(String, leading))
    of
        {error, Reason} ->
            error({Reason, String});
        {Integer, Rest} ->
            parse_integers(Rest, [Integer | Acc])
    end.

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
