-module(aoc2019_day23).
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
        problem => {2019, 23},
        examples => Examples,
        stable => false
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Program) ->
    flush(),
    Table =
        [
            {
                intcode:spawn(Program, [{input, [Address]}, {inputpid, self()}, {outputpid, self()}]),
                Address
            }
         || Address <- lists:seq(0, 49)
        ],
    Result = switch1(Table),
    [intcode:send(Halt, halt) || Halt <- proplists:get_keys(Table)],
    Result.

star2(Program) ->
    star2(Program, 1).
star2(Program, Timeout) ->
    flush(),
    Table =
        [
            {
                intcode:spawn(Program, [{input, [Address]}, {inputpid, self()}, {outputpid, self()}]),
                Address
            }
         || Address <- lists:seq(0, 49)
        ],

    try switch2(Table, sets:new(), [none, none], none, Timeout) of
        none ->
            io:format("none is not a valid result. ~n"),
            [intcode:send(Halt, halt) || Halt <- proplists:get_keys(Table)],
            star2(Program, Timeout + 10);
        Result ->
            [intcode:send(Halt, halt) || Halt <- proplists:get_keys(Table)],
            Result
    catch
        error:not_all_messages_receved ->
            io:format("Problem when checking for idle. ~n"),
            [intcode:send(Halt, halt) || Halt <- proplists:get_keys(Table)],
            star2(Program, Timeout + 10);
        error:{badmatch, Value}:Stack ->
            case hd(Stack) of
                {?MODULE, switch2, 5, Where} ->
                    io:format("Problem when routing ~p at ~p. ~n", [Value, Where]),
                    [intcode:send(Halt, halt) || Halt <- proplists:get_keys(Table)],
                    star2(Program, Timeout + 10);
                _ ->
                    error({{badmatch, Value}, Stack})
            end
    end.

read(File) ->
    intcode:from_file(File).

switch1(Table) ->
    receive
        {Pid, input} ->
            Address = proplists:get_value(Pid, Table),
            receive
                {save, Address, XY} ->
                    intcode:send(Pid, XY)
            after 0 ->
                intcode:send(Pid, [-1])
            end,
            switch1(Table);
        {Pid, [255]} ->
            {ok, [_X, Y]} = intcode:recvn(Pid, 2, 1000),
            Y;
        {Pid, [To]} ->
            {ok, XY} = intcode:recvn(Pid, 2, 1000),
            self() ! {save, To, XY},
            switch1(Table)
    after 10000 ->
        error(timeout)
    end.

switch2(Table, Inactive, [X, Y] = Message, Sent, Timeout) ->
    case sets:size(Inactive) of
        %% Idle?
        50 ->
            case is_idle(Timeout) of
                active ->
                    switch2(Table, sets:new(), Message, Sent, Timeout);
                idle ->
                    case Y == Sent of
                        true ->
                            Y;
                        false ->
                            {Pid, 0} = hd(Table),
                            intcode:send(Pid, [X, Y]),
                            switch2(Table, sets:new(), Message, Y, Timeout)
                    end
            end;
        %% not idle
        _ ->
            receive
                {Pid, input} ->
                    Address = proplists:get_value(Pid, Table),
                    receive
                        {save, Address, XY} ->
                            intcode:send(Pid, XY),
                            switch2(Table, Inactive, Message, Sent, Timeout)
                    after 0 ->
                        intcode:send(Pid, [-1]),
                        switch2(Table, sets:add_element(Pid, Inactive), Message, Sent, Timeout)
                    end;
                {Pid, [255]} ->
                    {ok, XY} = intcode:recvn(Pid, 2, 1000),
                    switch2(Table, sets:del_element(Pid, Inactive), XY, Sent, Timeout);
                {Pid, [To]} ->
                    {ok, XY} = intcode:recvn(Pid, 2, 1000),
                    self() ! {save, To, XY},
                    {Pid2, To} = lists:keyfind(To, 2, Table),
                    Inactive1 = sets:del_element(Pid2, Inactive),
                    switch2(Table, sets:del_element(Pid, Inactive1), Message, Sent, Timeout)
            after 10000 ->
                error(timeout)
            end
    end.

flush() ->
    receive
        _ ->
            flush()
    after 0 ->
        ok
    end.

is_idle(Timeout) ->
    is_idle([], [], sets:new(), Timeout).

is_idle(Messages, Input, Set, Timeout) ->
    receive
        {Pid, input} = Message ->
            is_idle(Messages, [Message | Input], sets:add_element(Pid, Set), Timeout);
        Message ->
            is_idle([Message | Messages], Input, Set, Timeout)
    after Timeout ->
        case Messages of
            [] ->
                case sets:size(Set) of
                    50 ->
                        [self() ! M0 || M0 <- lists:reverse(Input)],
                        idle;
                    _ ->
                        error(not_all_messages_receved)
                end;
            Messages ->
                [self() ! M1 || M1 <- lists:reverse(Messages)],
                [self() ! M2 || M2 <- lists:reverse(Input)],
                active
        end
    end.
