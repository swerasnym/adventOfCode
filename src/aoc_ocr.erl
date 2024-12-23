-module(aoc_ocr).

-export([run/0, maps/0]).
-export([decode/2]).

decode(Grid, Symbol) ->
    Letters = split_grid(Grid, Symbol),
    [match(L) || L <- Letters].

run() ->
    FilePath = filename:join(code:priv_dir(aoc), "ocr_letters.txt"),

    case filelib:is_file(FilePath) of
        false ->
            {error, {no_file, FilePath}};
        true ->
            Maps = generate_maps(FilePath),
            io:format("~kp", [Maps]),
            Maps
    end.

generate_maps(File) ->
    Blocks = tools:read_blocks(File, fun tools:parse_lines/1),

    maps:from_list([generate_maps(Info, Pattern) || [Info | Pattern] <- Blocks]).

generate_maps(Info, Pattern) ->
    [Height, Chars] = tools:parse_format(Info, "~d ~s"),
    FullGrid = tools:lists_to_grid(Pattern),
    Grids = split_grid(FullGrid, $#),
    Zipped = lists:zip(Chars, Grids),
    PosLetter = [{Pos, L} || {L, G} <- Zipped, Pos := $# <- G],
    LenLetter = [{maps:size(G), L} || {L, G} <- Zipped],
    {Height,
        maps:groups_from_list(
            fun({K, _}) -> K end,
            fun({_, V}) -> V end,
            PosLetter ++ LenLetter
        )}.

split_grid(Grid, S) ->
    Pixels = #{K => V || K := V <- Grid, V == S},
    {{Xmin, Xmax}, {Ymin, Ymax}} = tools:min_max_grid(Pixels),

    Translated = tools:translate_grid(Pixels#{max => {Xmax, Ymax}}, {-Xmin, -Ymin}),

    Int = intervals(0, find_gaps(Translated), []),
    {_, YMaxT} = maps:get(max, Translated),
    [tools:sub_grid(Translated, {X1, 0}, {X2, YMaxT}) || {X1, X2} <- Int].

find_gaps(#{max := {Xmax, Ymax}} = Grid) ->
    [X || X <- lists:seq(0, Xmax), all_blank(Grid, X, Ymax)] ++ [Xmax + 1].

intervals(_, [], Res) ->
    lists:reverse(Res);
intervals(S, [G | Rest], Res) when G == S ->
    intervals(S + 1, Rest, Res);
intervals(S, [G | Rest], Res) ->
    intervals(G + 1, Rest, [{S, G - 1} | Res]).

all_blank(Grid, X, Ymax) ->
    length([Y || Y <- lists:seq(0, Ymax), maps:is_key({X, Y}, Grid)]) == 0.

match(#{max := {_, Ymax}} = Letter) ->
    Reference = maps:get(Ymax + 1, maps()),
    Possibilities = [maps:get(Pos, Reference, []) || Pos <- maps:keys(Letter), Pos /= max],
    case tools:overlap([maps:get(maps:size(Letter), Reference, []) | Possibilities]) of
        [L] ->
            L;
        _ ->
            $?
    end.

maps() ->
    #{
        6 =>
            #{
                10 => "JLY",
                11 => "CI",
                12 => "FS",
                13 => "KOPUZ",
                14 => "G",
                15 => "AEHR",
                16 => "B",
                {0, 0} => "BEFHIKLPRUYZ",
                {0, 1} => "ABCEFGHKLOPRSUY",
                {0, 2} => "ABCEFGHKLOPRSU",
                {0, 3} => "ABCEFGHKLOPRU",
                {0, 4} => "ABCEFGHJKLOPRUZ",
                {0, 5} => "ABEFHIKLPRSZ",
                {1, 0} => "ABCEFGIOPRSZ",
                {1, 1} => "I",
                {1, 2} => "BEFHIKY",
                {1, 3} => "AIPRSZ",
                {1, 4} => "I",
                {1, 5} => "BCEGIJLOSUZ",
                {2, 0} => "ABCEFGIJOPRSZ",
                {2, 1} => "K",
                {2, 2} => "BEFHZ",
                {2, 3} => "AGKPRSY",
                {2, 4} => "KRY",
                {2, 5} => "BCEGIJLOSUYZ",
                {3, 0} => "EFHJKSUZ",
                {3, 1} => "ABCGHJOPRUZ",
                {3, 2} => "AHJOPRUY",
                {3, 3} => "ABGHJOU",
                {3, 4} => "ABCGHJOSU",
                {3, 5} => "AEGHKLRZ",
                {4, 0} => "Y",
                {4, 1} => "Y"
            },
        8 =>
            #{
                13 => "I",
                20 => "H",
                {0, 0} => "HI",
                {0, 1} => "H",
                {0, 2} => "H",
                {0, 3} => "H",
                {0, 4} => "H",
                {0, 5} => "H",
                {0, 6} => "H",
                {0, 7} => "HI",
                {1, 0} => "I",
                {1, 1} => "I",
                {1, 2} => "I",
                {1, 3} => "HI",
                {1, 4} => "I",
                {1, 5} => "I",
                {1, 6} => "I",
                {1, 7} => "I",
                {2, 0} => "I",
                {2, 3} => "H",
                {2, 7} => "I",
                {3, 3} => "H",
                {4, 0} => "H",
                {4, 1} => "H",
                {4, 2} => "H",
                {4, 3} => "H",
                {4, 4} => "H",
                {4, 5} => "H",
                {4, 6} => "H",
                {4, 7} => "H"
            },
        10 =>
            #{
                16 => "L",
                17 => "J",
                19 => "C",
                20 => "F",
                21 => "KXZ",
                22 => "P",
                25 => "AEGH",
                27 => "R",
                29 => "N",
                30 => "B",
                {0, 0} => "BEFHKLNPRXZ",
                {0, 1} => "BCEFGHKLNPRX",
                {0, 2} => "ABCEFGHKLNPR",
                {0, 3} => "ABCEFGHKLNPR",
                {0, 4} => "ABCEFGHKLNPR",
                {0, 5} => "ABCEFGHKLNPR",
                {0, 6} => "ABCEFGHKLNPR",
                {0, 7} => "ABCEFGHJKLNPRZ",
                {0, 8} => "ABCEFGHJKLNPRXZ",
                {0, 9} => "ABEFHKLNPRXZ",
                {1, 0} => "BCEFGPRZ",
                {1, 1} => "AN",
                {1, 2} => "NX",
                {1, 3} => "X",
                {1, 4} => "BEFHKPR",
                {1, 5} => "AK",
                {1, 6} => "XZ",
                {1, 7} => "X",
                {1, 9} => "BCEGJLZ",
                {2, 0} => "ABCEFGPRZ",
                {2, 3} => "KN",
                {2, 4} => "BEFHNPRX",
                {2, 5} => "AXZ",
                {2, 6} => "K",
                {2, 9} => "BCEGJLZ",
                {3, 0} => "ABCEFGJPRZ",
                {3, 2} => "K",
                {3, 4} => "BEFHPRXZ",
                {3, 5} => "AGNRX",
                {3, 6} => "N",
                {3, 7} => "K",
                {3, 9} => "BCEGJLZ",
                {4, 0} => "BCEFGJPRZ",
                {4, 1} => "AJK",
                {4, 2} => "JX",
                {4, 3} => "JXZ",
                {4, 4} => "BEFHJPR",
                {4, 5} => "AGJ",
                {4, 6} => "JRX",
                {4, 7} => "JNRX",
                {4, 8} => "GJKN",
                {4, 9} => "BCELZ",
                {5, 0} => "EFHJKNXZ",
                {5, 1} => "BCGHNPRXZ",
                {5, 2} => "ABHNPRZ",
                {5, 3} => "ABHNPR",
                {5, 4} => "AHN",
                {5, 5} => "ABGHN",
                {5, 6} => "ABGHN",
                {5, 7} => "ABGHN",
                {5, 8} => "ABCGHNRX",
                {5, 9} => "AEGHKLNRXZ"
            }
    }.
