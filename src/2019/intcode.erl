-module(intcode).

%% Parse
-export([from_list/1, from_string/1, from_file/1]).
%% Initiate
-export([
    set/3,
    set_options/2,
    set_input/2,
    set_output_pid/2,
    set_input_pid/2,
    set_exit_pid/2,
    set_timeout/2
]).
%% Start
-export([
    run/1,
    run_file/1,
    run_list/1,
    run_string/1,
    run/2,
    run_file/2,
    run_list/2,
    run_string/2
]).
%% Exit
-export([get/2, get_output/1, print/1]).
%% As a process
-export([
    spawn/1, spawn/2,
    send/2,
    recv/1, recv/2,
    recvn/2, recvn/3,
    interactive/1,
    interactive/2
]).
-export([analyze/1]).

%% erlfmt-ignore
-record(state,
        {ip = 0,              % Current Instruction pointer
         next_ip = 0,         % Next instruction pointer
         instruction,         % Instruction as an atom
         addresses,           % The raw addresses
         values,              % The values at the given addresses
         modes,               % The modes of the operation
         memory,              % The programs memory
         input = [],          % Input list
         output = [],         % Output stack
         outputpid = none,    % Pid to send output to
         inputpid = none,     % Pid to request input from
         exitpid = none,      % Pid to send final state to
         function = none,     % Function to run
         relative_base = 0,   % Relative base used in mode 2
         timeout = 10000}).      % How long to wait for messages in input

-define(INSTRUCTIONS, #{
    1 => {add, 3, fun add/1},
    2 => {multiply, 3, fun multiply/1},
    3 => {input, 1, fun input/1},
    4 => {output, 1, fun output/1},
    5 => {jump_if_true, 2, fun jump_if_true/1},
    6 => {jump_if_false, 2, fun jump_if_false/1},
    7 => {less_than, 3, fun less_than/1},
    8 => {equals, 3, fun equals/1},
    9 => {relative_base_offset, 1, fun relative_base_offset/1},
    99 => {halt, 0, fun halt/1}
}).

%%------------------------------------------------------------------------------
%% Intcode function calls
%%------------------------------------------------------------------------------
call(#state{function = Function} = State) ->
    Function(State).

add(#state{values = [Term1, Term2, _], addresses = [_, _, To]} = State) ->
    set(Term1 + Term2, To, State).

multiply(#state{values = [Factor1, Factor2, _], addresses = [_, _, To]} = State) ->
    set(Factor1 * Factor2, To, State).

input(
    #state{
        addresses = [To],
        input = [],
        inputpid = Pid,
        timeout = Timeout
    } =
        State0
) ->
    send(Pid, input),
    case recv(Pid, Timeout) of
        [Value | Rest] ->
            State = set(Value, To, State0),
            State#state{input = Rest};
        halt ->
            %% io:fwrite("~p recived halt halting~n", [self()]),
            halt;
        timeout ->
            io:fwrite("~p recived timeout, have you forgotten to send halt?~n", [self()]),
            halt
    end;
input(#state{addresses = [To], input = [Value | Rest]} = State0) ->
    State = set(Value, To, State0),
    State#state{input = Rest}.

output(
    #state{
        values = [Value],
        output = Output,
        outputpid = Pid
    } =
        State
) ->
    send(Pid, [Value]),
    State#state{output = [Value | Output]}.

jump_if_true(#state{values = [Value, To]} = State) ->
    case Value of
        0 ->
            State;
        Value ->
            State#state{next_ip = To}
    end.

jump_if_false(#state{values = [Value, To]} = State) ->
    case Value of
        0 ->
            State#state{next_ip = To};
        Value ->
            State
    end.

less_than(#state{values = [Term1, Term2, _], addresses = [_, _, To]} = State) when
    Term1 < Term2
->
    set(1, To, State);
less_than(#state{values = [_, _, _], addresses = [_, _, To]} = State) ->
    set(0, To, State).

equals(#state{values = [Term, Term, _], addresses = [_, _, To]} = State) ->
    set(1, To, State);
equals(#state{values = [_, _, _], addresses = [_, _, To]} = State) ->
    set(0, To, State).

relative_base_offset(#state{values = [Value], relative_base = Rel} = State) ->
    State#state{relative_base = Rel + Value}.

halt(#state{outputpid = Pid}) ->
    send(Pid, halt).

%%------------------------------------------------------------------------------
%% Code parsing functions
%%------------------------------------------------------------------------------
from_list(List) ->
    #state{memory = array:from_list(List, 0)}.

from_string(String) ->
    List = string:split(String, ",", all),
    F = fun(S) ->
        case string:to_integer(string:trim(S, both)) of
            {Int, <<>>} ->
                Int;
            {Int, []} ->
                Int;
            _ ->
                error({invalid_format, S})
        end
    end,
    from_list(lists:map(F, List)).

from_file(File) ->
    {ok, String} = file:read_file(File),
    from_string(String).

%%------------------------------------------------------------------------------
%% Settings
%%------------------------------------------------------------------------------
set(Value, Address, #state{memory = Memory} = State) ->
    State#state{memory = array:set(Address, Value, Memory)}.

set_options([], State) ->
    State;
set_options([Option | Options], State) ->
    State1 =
        case Option of
            {set, Value, Address} ->
                set(Value, Address, State);
            {input, List} ->
                set_input(List, State);
            {outputpid, Pid} ->
                set_output_pid(Pid, State);
            {inputpid, Pid} ->
                set_input_pid(Pid, State);
            {exitpid, Pid} ->
                set_exit_pid(Pid, State);
            {timeout, Timeout} ->
                set_timeout(Timeout, State)
        end,

    set_options(Options, State1).

set_input(List, State) ->
    State#state{input = List}.

set_output_pid(Pid, State) ->
    State#state{outputpid = Pid}.

set_input_pid(Pid, State) ->
    State#state{inputpid = Pid}.

set_exit_pid(Pid, State) ->
    State#state{exitpid = Pid}.

set_timeout(Timeout, State) ->
    State#state{timeout = Timeout}.

%%------------------------------------------------------------------------------
%% Run on in the same process
%%------------------------------------------------------------------------------
run(#state{exitpid = Pid} = State0) ->
    State1 = step_ip(State0),
    %% print(State0),
    case call(State1) of
        halt ->
            send(Pid, {exit, State1}),
            State1;
        State ->
            run(State)
    end.

run(State0, Options) ->
    State = set_options(Options, State0),
    run(State).

run_string(String, Options) ->
    run(from_string(String), Options).

run_string(String) ->
    run(from_string(String)).

run_list(List, Options) ->
    run(from_list(List), Options).

run_list(List) ->
    run(from_list(List)).

run_file(File, Options) ->
    run(from_file(File), Options).

run_file(File) ->
    run(from_file(File)).

%%------------------------------------------------------------------------------
%% Get information from finnished program
%%------------------------------------------------------------------------------
get(Address, #state{memory = Memory}) ->
    array:get(Address, Memory).

get_output(#state{output = Output}) ->
    lists:reverse(Output).

print(#state{
    ip = Ip,
    next_ip = Next,
    instruction = Instruction,
    addresses = Address,
    values = Values,
    input = Input,
    memory = Memory,
    relative_base = Rel
}) ->
    io:format(
        "ip = ~w~nnext_ip = ~w~ninstruction = ~w~naddresses = ~w~nvalues "
        "= ~w~ninput = ~w~nrelative_base = ~w~n",
        [Ip, Next, Instruction, Address, Values, Input, Rel]
    ),

    io:format("memory= ~w~n~n", [array:to_list(Memory)]).

%%------------------------------------------------------------------------------
%% Spawn and interact with a program as a separate process
%%------------------------------------------------------------------------------
spawn(State) ->
    spawn_link(intcode, run, [State]).

spawn(State, Options) ->
    spawn_link(intcode, run, [State, Options]).

send(none, Value) ->
    Value;
send(To, Value) ->
    To ! {self(), Value},
    Value.

recv(Pid) ->
    recv(Pid, infinity).

recv(none, Timeout) ->
    receive
        {_, Value} ->
            Value
    after Timeout ->
        timeout
    end;
recv(Pid, Timeout) ->
    receive
        {Pid, Value} ->
            Value
    after Timeout ->
        timeout
    end.

recvn(Pid, N) ->
    {N0, Acc} = recvn_buffer(Pid, N),
    recvn(Pid, N0, Acc, infinity).

recvn(Pid, N, Timeout) ->
    {N0, Acc} = recvn_buffer(Pid, N),
    recvn(Pid, N0, Acc, Timeout).

recvn(_Pid, 0, Acc, _Timeout) ->
    {ok, Acc};
recvn(Pid, N, Acc, Timeout) ->
    case recv(Pid, Timeout) of
        Value when is_list(Value) andalso length(Value) =< N ->
            recvn(Pid, recvn_dec(N, length(Value)), Acc ++ Value, Timeout);
        Value when is_list(Value) ->
            {Head, Extra} = lists:split(N, Value),
            self() ! {Pid, recvn_buffer, Extra},
            {ok, Acc ++ Head};
        input ->
            {input, Acc};
        halt ->
            {halt, Acc};
        timeout ->
            {timeout, Acc};
        {exit, State} ->
            {exit, State, Acc}
    end.

interactive(Program) ->
    Pid = intcode:spawn(
        Program,
        [
            {inputpid, self()},
            {outputpid, self()},
            {exitpid, self()},
            {timeout, 600000}
        ]
    ),
    shell(Pid, nothing).

interactive(Program, Options) ->
    Pid = intcode:spawn(
        Program,
        [
            {inputpid, self()},
            {outputpid, self()},
            {exitpid, self()},
            {timeout, 600000}
        ] ++
            Options
    ),
    shell(Pid, nothing).

shell(Pid, Last) ->
    case intcode:recvn(Pid, 1) of
        {ok, Data} ->
            try
                io:format(Data)
            catch
                error:_Error ->
                    io:format("~p~n", [Data])
            end,
            shell(Pid, Data);
        {input, Prompt} ->
            Line = io:get_line(Prompt),
            intcode:send(Pid, Line),
            shell(Pid, Last);
        {halt, Output} ->
            try
                io:format(Output)
            catch
                error:_Error ->
                    io:format("~p~n", [Output])
            end,
            Last;
        {exit, State, Output} ->
            {exit, State, Output}
    end.

analyze(#state{memory = Memory} = State) ->
    analyze(State, 0, array:size(Memory)).

analyze(State, Ip, Max) when Ip < Max ->
    Intcode = get(Ip, State),
    case maps:get(Intcode rem 100, ?INSTRUCTIONS, data) of
        {Instruction, Parameters, _Function} ->
            Modes = modes(Intcode div 100, Parameters),

            AddrValue = [get(Pos, State) || Pos <- lists:seq(Ip + 1, Ip + Parameters)],

            Format = [format(V) || V <- lists:zip(Modes, AddrValue)],

            io:format("~p: ~p ~s ~s~n", [Ip, Intcode, Instruction, string:join(Format, " ")]),

            analyze(State, Ip + Parameters + 1, Max);
        data ->
            io:format("~p: ~p data ~n", [Ip, Intcode]),
            analyze(State, Ip + 1, Max)
    end;
analyze(_, _, _) ->
    ok.

format({Mode, AddrValue}) ->
    case Mode of
        0 ->
            "[" ++ integer_to_list(AddrValue) ++ "]";
        1 ->
            integer_to_list(AddrValue);
        2 ->
            "{" ++ integer_to_list(AddrValue) ++ "}"
    end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
step_ip(#state{next_ip = Ip} = State) ->
    Intcode = get(Ip, State),
    {Instruction, Parameters, Function} = maps:get(Intcode rem 100, ?INSTRUCTIONS),
    Modes = modes(Intcode div 100, Parameters),
    Addresses = get_addresses(Ip, Modes, State),
    Values = get_values(Addresses, State),

    State#state{
        ip = Ip,
        next_ip = Ip + Parameters + 1,
        instruction = Instruction,
        addresses = Addresses,
        values = Values,
        modes = Modes,
        function = Function
    }.

recvn_buffer(Pid, N) ->
    receive
        {Pid, recvn_buffer, Value} when length(Value) =< N ->
            {recvn_dec(N, length(Value)), Value};
        {Pid, recvn_buffer, Value} ->
            {Head, Extra} = lists:split(N, Value),
            self() ! {Pid, recvn_buffer, Extra},
            {0, Head}
    after 0 ->
        {N, []}
    end.

recvn_dec(all, _) ->
    all;
recvn_dec(N, Dec) ->
    N - Dec.

get_addresses(_, [], _) ->
    [];
get_addresses(Ip, Modes, #state{relative_base = Rel} = State) ->
    Positions = lists:seq(Ip + 1, Ip + length(Modes)),
    [
        case Mode of
            0 ->
                get(Pos, State);
            1 ->
                Pos;
            2 ->
                get(Pos, State) + Rel
        end
     || {Pos, Mode} <- lists:zip(Positions, Modes)
    ].

get_values(Addresses, State) ->
    [get(Address, State) || Address <- Addresses].

modes(Modes, Parameters) ->
    modes(Modes, Parameters, []).

modes(_, 0, Acc) ->
    lists:reverse(Acc);
modes(Modes, Parameters, Acc) ->
    modes(Modes div 10, Parameters - 1, [Modes rem 10 | Acc]).
