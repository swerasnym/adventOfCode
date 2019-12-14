-module(day13).
-export([run/2]).


-record(state, 
      {screen = #{},
       ball = {0,0},
       paddle = 0,
       score = 0}).

run(Star, File) ->
    Program = intcode:from_file(File),
    case Star of
	star1 ->
	    star1(Program);
	star2 ->
	    star2(Program);
	_ ->
	    Star1 = star1(Program),
	    Star2 = star2(Program),
	    {Star1, Star2}
    end.

star1(Program) -> 
    Program1 = intcode:set_output_pid(self(), Program),
    
    Pid = spawn_link(intcode, run, [Program1]),
    #state{screen=Screen} = arcade(#state{}, Pid),
    paint(Screen),
    length([X || X <- maps:values(Screen), X == block]).

star2(Program) ->
    
    Program1 = intcode:set_input_output_pid(self(), self(), Program),
    Program2 = intcode:set(2, 0, Program1),
    Pid = spawn_link(intcode, run, [Program2]),
    #state{screen=Screen, score= Score} = arcade(#state{}, Pid),
    paint(Screen),
    Score.


arcade(State, Pid) ->
    receive
	[X] ->
	    X,
	    Y = receive
		    [Data1] ->
			Data1
		after 1000 ->
			error(no_y)
		end,

	    Id = receive
		     [Data2] ->
			 Data2
		 after 1000 ->
			 error(no_id)
		 end,
	    State1 = output(State, X, Y, Id),
	    arcade(State1, Pid);
	input ->
	    State1 = input(State, Pid),
	    arcade(State1, Pid);
	halt ->
	    State
    after 1000 ->
	    error(no_x)
    end.



input(#state{screen=_Screen, paddle = {Px, _Py}, ball = {Bx, _By}} = State, Pid) ->
    %% paint(Screen),
    Data = case Px - Bx of
	       0 ->
		   [0];
	       Value when Value < 0 ->
		   [1];
	       Value when Value > 0 ->
		   [-1]
	   end,
    Pid ! Data,
    State.

output(#state{} = State, -1, 0, Score) -> 
   %%  io:fwrite("Score: ~p~n", [Score]),
    State#state{score=Score};

output(#state{screen=Screen} = State, X, Y, Id) ->
    case Id of
	0 ->
	    State#state{screen= Screen#{{X,Y} =>empty}};
	1->
	    State#state{screen= Screen#{{X,Y} =>wall}};
	2->
	    State#state{screen= Screen#{{X,Y} =>block}};
	3->
	    State#state{screen= Screen#{{X,Y} =>paddle}, paddle = {X,Y}};
	4->
	    State#state{screen= Screen#{{X,Y} => ball}, ball = {X,Y}}
    end.



paint(Screen) ->
    Xs = [X || {X,_} <- maps:keys(Screen)],
    Ys = [Y || {_,Y} <- maps:keys(Screen)],
	  
    
    [paint({X,Y}, Screen, lists:max(Xs)) || Y <- lists:seq(lists:min(Ys), lists:max(Ys)), 
					  X <- lists:seq(lists:min(Xs), lists:max(Xs))],
    ok.

paint({X,_} = Pos, Screen, X) ->
    case maps:get(Pos, Screen,black) of
	empty ->
	    io:format(" ~n",[]);

	wall->
	    io:format("%~n",[]);
	block->

	    io:format("*~n",[]);
	paddle->

	    io:format("|~n",[]);
	ball ->

	    io:format("o~n",[])
    end;
paint(Pos, Screen, _) ->
    case maps:get(Pos, Screen,black) of
	empty ->
	    io:format(" ",[]);

	wall->
	    io:format("%",[]);
	block->

	    io:format("*",[]);
	paddle->

	    io:format("_",[]);
	ball ->

	    io:format("o",[])
    end.   
