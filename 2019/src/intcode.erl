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
	 output = [],
	 outputpid = none,
	 function = none
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
	 set_output_pid/2,
	 get_output/1,
	 print/1]).


-define(INSTRUCTIONS,
	#{1 => {add, 3, fun add/1},
	  2 => {multiply, 3, fun multiply/1},
	  3 => {input, 1, fun input/1},
	  4 => {output, 1, fun output/1},
	  5 => {jump_if_true, 2, fun jump_if_true/1},
	  6 => {jump_if_false, 2, fun jump_if_false/1},
	  7 => {less_than, 3, fun less_than/1},
	  8 => {equals, 3, fun equals/1},

	  guess1 => {subtract, 3, fun subtract/1},
	  guess2 => {divide, 3, fun divide/1},
	  guess3 => {reminder, 3, fun reminder/1},
	  guess4 => {jump, 3, fun jump/1},

	  99 => {halt, 0, fun halt/1}}).


-define(vva(V1, V2, A), #state{values = [V1, V2, _], addresses = [_,_,A]}).


call(#state{function = Function} = State) ->
    Function(State).

add(?vva(Term1,Term2, To) = State) ->
    set(Term1 + Term2, To, State).

multiply(?vva(Factor1, Factor2, To) = State) ->
    set(Factor1 * Factor2, To, State).


input(#state{addresses = [To], input = [] } = State0) ->
    receive
	[Value|Rest] ->
	    State = set(Value, To , State0),
	    State#state{input=Rest}
    end;
input(#state{addresses = [To], input = [Value|Rest] } = State0) ->
    State = set(Value, To , State0),
    State#state{input=Rest}.


output(#state{values = [Value], output = Output, outputpid = none} = State) ->
    State#state{output=[Value | Output]};
output(#state{values = [Value], output = Output, outputpid = Pid } = State) ->
    Pid ! [Value],
    State#state{output=[Value | Output]}.

jump_if_true(#state{values = [Value, To]} = State ) ->
    case Value of
	0 ->
	    State;
	Value ->
	    State#state{next_ip = To}
    end.

jump_if_false(#state{values = [Value, To]} = State ) ->
    case Value of
	0 ->
	    State#state{next_ip = To};
	Value ->
	    State
    end.

less_than(?vva(Term1,Term2, To) = State) when Term1 < Term2 ->
    set(1, To, State);
less_than(?vva(_, _, To) = State) ->
    set(0, To, State).

equals(?vva(Term,Term, To) = State) ->
    set(1, To, State);
equals(?vva(_, _, To) = State) ->
    set(0, To, State).

subtract(?vva(Term1, Term2, To) = State) ->
    set(Term1 - Term2, To, State).


divide(?vva(Nominator, Denominator, To) = State) ->
    set(Nominator div  Denominator, To, State).

reminder(?vva(Nominator, Denominator, To) = State) ->
    set(Nominator rem  Denominator, To, State).

jump(#state{values = [To]} = State)->
    State#state{next_ip = To}.

halt(_) ->
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

set_output_pid(Pid, State) ->
    State#state{outputpid = Pid}.


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
    {Instruction, Parameters, Function} = maps:get(Intcode rem 100, ?INSTRUCTIONS),
    Modes = mode(Intcode div 100, Parameters),
    Addresses = get_addresses(Ip, Modes, State),
    Values = get_values(Addresses, State),

    State#state{ip = Ip,
		next_ip = Ip + Parameters + 1,
		instruction = Instruction,
		addresses = Addresses,
		values = Values,
		mode = Modes,
		function = Function}.



mode(Mode, Parameters) ->
   mode(Mode, Parameters, []).

mode(_, 0, Acc) ->
    lists:reverse(Acc);
mode(Mode, Parameters, Acc) ->
    mode(Mode div 10, Parameters - 1, [Mode rem 10 | Acc]).
