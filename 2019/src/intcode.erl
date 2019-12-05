-module(intcode).

-record(state,
	{
	 ip = 0,              % Current Instruction pointer
	 next_ip = 0,         % Next Instruction pointer
	 instruction,         % instruction as an atom
	 addresses,           % The raw addresses
	 values,              % The value at the given address
	 mode,                % The mode of the operation
	 memory,
	 input = [],
	 output = []
	}).

% Main api
-export([set/3,
	 get/2,
	 from_list/1,
	 from_string/1,
	 from_file/1,
	 run/1,
	 run_file/1,
	 run_file/2,
	 run_list/1,
	 run_list/2,
	 run_string/1,
	 run_string/2,
	 set_input/2,
	 get_output/1,
	 print/1]).

%% Testing api
-export(
   [
    instruction/1,
    parameters/1,
    optcode/1,
    call/1,
    call/2,
    get_addresses/3,
    get_values/2,
    mode/2
   ]).


instruction(1) ->
    add;
instruction(2) ->
    multiply;
instruction(3) ->
    input;
instruction(4) ->
    output;
instruction(5) ->
    jump_if_true;
instruction(6) ->
    jump_if_false;
instruction(7) ->
    less_than;
instruction(8) ->
    equals;
instruction(99) ->
    halt.

parameters(add) ->
    3;
parameters(multiply) ->
    3;
parameters(input) ->
    1;
parameters(output) ->
    1;
parameters(jump_if_true) ->
    2;
parameters(jump_if_false) ->
    2;
parameters(less_than) ->
    3;
parameters(equals) ->
    3;
parameters(subtract) -> %% Guess
    3;
parameters(divide) -> %% Guess
    3;
parameters(reminder) -> %% Guess
    3;
parameters(jump) -> %% Guess
    1;
parameters(halt) ->
    0.


optcode(add)->
    1;
optcode(multiply) ->
    2;
optcode(input) ->
    3;
optcode(output) ->
    4;
optcode(jump_if_true) ->
    5;
optcode(jump_if_false) ->
    6;
optcode(less_than) ->
    7;
optcode(equals) ->
    8;
optcode(halt) ->
    99.


call(#state{instruction = Instruction} = State) ->
    call(Instruction, State).

call(add, #state{values = [Term1, Term2, _], addresses = [_,_,To]} = State) ->
    set(Term1 + Term2, To, State);

call(multiply, #state{values = [Factor1, Factor2, _], addresses = [_,_,To]} = State) ->
    set(Factor1 * Factor2, To, State);

call(input, #state{addresses = [To], input = [Value|Rest] } = State0) ->
    State = set(Value, To , State0),
    State#state{input=Rest};


call(output, #state{values = [Value], output = Output } = State) ->
    State#state{output=[Value | Output]};



call(jump_if_true, #state{values = [Value, To]} = State ) ->
    case Value of
	0 ->
	    State;
	Value ->
	    State#state{next_ip = To}
    end;

call(jump_if_false, #state{values = [Value, To]} = State ) ->
    case Value of
	0 ->
	    State#state{next_ip = To};
	Value ->
	    State
    end;



call(less_than, #state{values = [Term1, Term2, _], addresses = [_,_,To]} = State) when Term1 < Term2 ->
    set(1, To, State);

call(less_than, #state{addresses = [_,_,To]} = State) ->
    set(0, To, State);

call(equals, #state{values = [Term, Term, _], addresses = [_,_,To]} = State) ->
    set(1, To, State);
call(equals, #state{addresses = [_,_,To]} = State) ->
    set(0, To, State);



call(subtract, #state{values = [Term1, Term2, _], addresses = [_,_,To]} = State) ->
    set(Term1 - Term2, To, State);



call(divide, #state{values = [Nominator, Denominator, _], addresses = [_,_,To]} = State) ->
    set(Nominator div  Denominator, To, State);

call(reminder, #state{values = [Nominator, Denominator, _], addresses = [_,_,To]} = State) ->
    set(Nominator rem  Denominator, To, State);


call(jump, #state{addresses = [To]} = State)->
    State#state{next_ip = To};



call(halt, _) ->
    halt.


print(#state{
	 ip = Ip,
	 next_ip = Next,
	 instruction = Instruction,
	 addresses = Address,
	 values = Values,
	 input = Input,
	 memory = Memory
	}) ->
    io:format("ip = ~w~n"
	      "next_ip = ~w~n"
	      "instruction = ~w~n"
	      "addresses = ~w~n"
	      "values = ~w~n"
	      "input = ~w~n",
	      [Ip, Next, Instruction, Address, 	Values, Input ]),

    io:format("memory= ~w~n~n", [array:to_list(Memory)]).


set(Value, Address, #state{memory = Memory} = State) ->
    State#state{memory = array:set(Address, Value, Memory)}.


set_input(List, State) ->
    State#state{input = List}.


get(Address, #state{memory = Memory}) ->
    array:get(Address, Memory).


get_output(#state{output = Output}) ->
    lists:reverse(Output).


run_string(String, Inputs) ->
    Program0 = from_string(String),
    Program = set_input(Inputs, Program0),
    Result = run(Program),
    get_output(Result).

run_string(String) ->
    run(from_string(String)).

run_list(List, Inputs) ->
    Program0 = from_list(List),
    Program = set_input(Inputs, Program0),
    Result = run(Program),
    get_output(Result).

run_list(List) ->
    run(from_list(List)).

run_file(File, Inputs) ->
    Program0 = from_file(File),
    Program = set_input(Inputs, Program0),
    Result = run(Program),
    get_output(Result).

run_file(File) ->
    run(from_file(File)).

run(State0) ->
    State1 = step_ip(State0),
    case call(State1) of
	halt ->
	    State1;
	State ->
	    run(State)
    end.

from_list(List) ->
    #state{memory = array:from_list(List,nil)}.

from_string(String) ->
    String1 = string:split(String, "\n", all),
    List = string:split(String1, ",", all),
    F = fun (S) ->
		case string:to_integer(S) of
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
    {ok,String} = file:read_file(File),
    from_string(String).

%% Helper functions

get_addresses(_, [], _) ->
    [];

get_addresses(Ip, Modes, State) ->
    Positions = lists:seq(Ip + 1, Ip + length(Modes)),
    [case Mode of
	 0 ->
	     get(Pos, State);
	 1 ->
	     Pos
     end
     || {Pos, Mode} <- lists:zip(Positions, Modes)].

get_values(Addresses, State) ->
    [get(Address, State) || Address <- Addresses].


step_ip(#state{next_ip = Ip} = State) ->

    Intcode = get(Ip, State),
    Instruction = instruction(Intcode rem 100),
    Parameters = parameters(Instruction),
    Modes = mode(Intcode div 100, Parameters),
    Addresses = get_addresses(Ip, Modes, State),
    Values = get_values(Addresses, State),

    State#state{ip = Ip,
		next_ip = Ip + Parameters + 1,
		instruction = Instruction,
		addresses = Addresses,
		values = Values,
		mode = Modes}.



mode(Mode, Parameters) ->
   mode(Mode, Parameters, []).

mode(_, 0, Acc) ->
    lists:reverse(Acc);
mode(Mode, Parameters, Acc) ->
    mode(Mode div 10, Parameters - 1, [Mode rem 10 | Acc]).

