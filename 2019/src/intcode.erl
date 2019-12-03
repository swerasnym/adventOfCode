-module(intcode).

-record(state,
	{
	 ip = 0,
	 next_ip = 0,
	 instruction = none,
	 addresses = none,
	 values = none,
	 memory = #{}
	}).

% Main api 
-export([set/3,
	 get/2,
	 read/1,
	 run/1,
	 run_file/1]).

%% Testing api
-export(
   [
    instruction/1,
    parameters/1,
    optcode/1,
    call/1,
    call/2,
    get_addresses/3,
    get_values/2    
   ]).

instruction(1) ->
    add;
instruction(2) ->
    multiply;
instruction(99) ->
    halt.

parameters(add) ->
    3;
parameters(subtract) -> %% Guess
    3;
parameters(multiply) ->
    3;
parameters(divide) -> %% Guess
    3;
parameters(reminder) -> %% Guess
    3;
parameters(jump) -> %% Guess
    1;
parameters(jump_not_zero) -> %% Guess
    2;

parameters(halt) ->
    0.


optcode(add)->
    1;
optcode(multiply) ->
    2;
optcode(halt) ->
    99.


call(#state{instruction = Instruction} = State) ->
    call(Instruction, State).

call(add, #state{values = [Term1, Term2, _], addresses = [_,_,To]} = State) ->
    set(Term1 + Term2, To, State);

call(subtract, #state{values = [Term1, Term2, _], addresses = [_,_,To]} = State) ->
    set(Term1 - Term2, To, State);

call(multiply, #state{values = [Factor1, Factor2, _], addresses = [_,_,To]} = State) ->
    set(Factor1 * Factor2, To, State);

call(divide, #state{values = [Nominator, Denominator, _], addresses = [_,_,To]} = State) ->
    set(Nominator div  Denominator, To, State);

call(reminder, #state{values = [Nominator, Denominator, _], addresses = [_,_,To]} = State) ->
    set(Nominator rem  Denominator, To, State);


call(jump, #state{addresses = [To]} = State)->
    State#state{next_ip = To};

call(jump_not_zero, #state{values = [Value, _], addresses = [_,To]} = State ) ->
    case Value of
	0 ->
	    State;
	Value ->
	    State#state{next_ip = To}
    end;

call(halt, _) ->
    halt.


print(State) ->
    print(State, 0).

print(#state{
	 ip = Ip,
	 next_ip = Next,
	 instruction = Instruction,
	 addresses = Address,
	 values = Values
	} = State, 0) ->
    io:format("ip = ~w~nnext_ip = ~w~ninstruction = ~w~naddresses = ~w~nvalues = ~p~n",
	      [Ip, Next, Instruction, Address, 	Values ]),

    io:format("memory= ~w", [get(0, State)]),
    print(State, 1);

print(State, N) ->
    case get(N, State) of
	invalid_address ->
	    io:format("~n");
	Value ->
	    io:format(",~w", [Value]),
	    print(State, N+1)
    end.

set(Value, Address, #state{memory = Memory} = State) ->
    State#state{memory = Memory#{Address => Value}}.

get(Address, #state{memory = Memory}) ->
    maps:get(Address, Memory, invalid_address).




run_file(File) ->
    {ok, Device} = file:open(File, [read]),
    Program = read(Device),
    run(Program).

run(State0) ->
    State1 = step_ip(State0),
    case call(State1) of
	halt ->
	    State1;
	State ->
	    run(State)
    end.



read(Device) ->
    {ok, [P]} = io:fread(Device, [], "~d"),
    Program = read(Device, #{0 => P}, 1),
    #state{memory=Program}.

read(Device, Acc, Counter) ->
    case io:fread(Device, [], ",~d") of
	eof ->
	    Acc;			
	{ok, [D]} ->
	    read(Device, Acc#{Counter => D} , Counter +1);
	{error, What} ->
	    error({error,What, Acc, Counter})
    end.


%% Helper functions


get_addresses(_, 0, _) ->
    [];
get_addresses(Ip, Parameters, State) ->
    [get(Pos, State) || Pos <- lists:seq(Ip + 1, Ip + Parameters)].

get_values(Addresses, State) ->
    [get(Address, State) || Address <- Addresses].
    

step_ip(#state{next_ip = Ip} = State) ->
    
    Instruction = instruction(get(Ip, State)),
    Parameters = parameters(Instruction),
    Addresses = get_addresses(Ip, Parameters, State),
    Values = get_values(Addresses, State),

    State#state{ip = Ip, 
		next_ip = Ip + Parameters + 1,
		instruction = Instruction,
		addresses = Addresses,
		values = Values}.




