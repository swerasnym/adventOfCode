-module(aoc2019_day25).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 25},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    intcode:from_file(File).

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
    {done, "Align the warp drive!"}.

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
