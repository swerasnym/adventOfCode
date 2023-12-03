-module(aoc2019_day25).

-export([run/2]).

run(Star, File) ->
    Program = intcode:from_file(File),
    case Star of
        star1 ->
            star1(Program);
        star2 ->
            star2(Program);
        int ->
            intcode:interactive(Program),
            ok;
        _ ->
            Star1 = star1(Program),
            Star2 = star2(Program),
            {Star1, Star2}
    end.

star1(Program) ->
    Instructions =
        [
            "north",
            "north",
            "north",
            "take mutex",
            "south",
            "south",
            "east",
            "north",
            "take loom",
            "south",
            "west",
            "south",
            "east",
            "take semiconductor",
            "east",
            "take ornament",
            "west",
            "west",
            "west",
            "west",
            "take sand",
            "south",
            "east",
            "take asterisk",
            "north",
            "take wreath",
            "south",
            "west",
            "north",
            "north",
            "take dark matter",
            "east",
            "inv",
            "drop semiconductor",
            "drop loom",
            "drop mutex",
            "drop sand",
            "drop asterisk",
            "drop wreath",
            "drop dark matter",
            "drop ornament",
            "inv",
            "east",
            ""
        ],

    Pid = intcode:spawn(
        Program,
        [
            {inputpid, self()},
            {outputpid, self()},
            {input, string:join(Instructions, "\n")}
        ]
    ),

    {input, String} = intcode:recvn(Pid, all),
    io:format(String),
    Code = find_weight(Pid),
    intcode:send(Pid, halt),
    list_to_integer(Code).

star2(_Program) ->
    "Press done since you got 49 stars!".

find_weight(Pid) ->
    Items =
        [
            "semiconductor",
            "loom",
            "mutex",
            "sand",
            "asterisk",
            "wreath",
            "dark matter",
            "ornament"
        ],
    find_weight(Pid, generate(Items)).

find_weight(Pid, [Items | Rest]) ->
    Input1 = string:join(["take " ++ Item || Item <- Items], "\n") ++ "\n",
    intcode:send(Pid, Input1 ++ "inv\n" ++ "east\n"),
    case intcode:recvn(Pid, all) of
        {input, String} ->
            [_, Voice] = string:split(String, "A loud, robotic voice says \""),

            case Voice of
                "Alert! Droids on this ship are heavier than the detected value!" ++ _ ->
                    Input2 = string:join(["drop " ++ Item || Item <- Items], "\n") ++ "\n",
                    intcode:send(Pid, Input2),
                    intcode:recvn(Pid, all),
                    find_weight(Pid, Rest);
                "Alert! Droids on this ship are lighter than the detected value!" ++ _ ->
                    Input2 = string:join(["drop " ++ Item || Item <- Items], "\n") ++ "\n",
                    intcode:send(Pid, Input2),
                    intcode:recvn(Pid, all),
                    find_weight(Pid, Rest)
            end;
        {halt, String} ->
            io:format(String),
            [_, Code1] = string:split(String, "You should be able to get in by typing "),
            [Code, _] = string:split(Code1, " on the keypad at the main airlock."),
            Code
    end.

generate([]) ->
    [[]];
generate([H | T]) ->
    PT = generate(T),
    [[H | X] || X <- PT] ++ PT.
