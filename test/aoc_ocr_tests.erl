-module(aoc_ocr_tests).
-include_lib("eunit/include/eunit.hrl").

generates_correct_map_test() ->
    ?assertEqual(
        aoc_ocr:run(),
        aoc_ocr:maps(),
        "New letters added, re-generate maps()!"
    ).

decode_test_() ->
    File = filename:join(code:priv_dir(aoc), "ocr_letters.txt"),
    Blocks = tools:read_blocks(File, fun parse/1),
    [?_assertEqual(Text, aoc_ocr:decode(Grid, $#)) || {Text, Grid} <- Blocks].

parse(Block) ->
    [Head, Grid] = string:split(Block, "\n"),
    [_, Text] = tools:parse_format(Head, "~d ~s"),
    {Text, tools:parse_grid(Grid)}.
