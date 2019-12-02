-module(day2).
-export([run/2,star2/1, run_program/1, eval/2]).

run(Star, File) ->
    {ok, Device} = file:open(File, [read]),
    Data = read_data(Device),
    case Star of
	star1 ->
	    star1(Data);
	star2 ->
	    star2(Data);
	_ -> 
	    Star1 = star1(Data),
	    Star2 = star2(Data),
	    {Star1, Star2}
    end.

star1(Data) ->
    run_program(Data#{1 => 12, 2 => 2}).

star2(Data) ->
    
    F = fun (Noun, Verb) ->
		case run_program(Data#{1 => Noun, 2 => Verb}) of
		    19690720 ->
			done;
		    _ ->
		       continue
		end	
	end,
 
	    

    seek(F,0,0).


seek(_,_,100) ->
    noresult;
seek(F, 100, V) -> 
   seek(F,0, V+1);
seek(F, N, V) ->
    case F(N,V) of
	done -> 
	    100*N+V;
	continue ->
	    seek(F, N+1, V)
    end.


    
read_data(Device) ->
    {ok, [D]} = io:fread(Device, [], "~d"),
    read_data(Device, #{0 => D}, 1).

read_data(Device, Acc, Counter) ->
    case io:fread(Device, [], ",~d") of
	eof ->
	    Acc;			
	{ok, [D]} ->
	    read_data(Device, Acc#{Counter => D} , Counter +1);
	{error, What} ->
	    error({error,What, Acc, Counter})
    end.




run_program(Data)->
    #{0 := Res} = run_program(0, Data),
    Res.

run_program(Pc, Data)->
    A = Pc+1,
    B = Pc+2,
    C = Pc+3,
   
    #{Pc := Command, A := In1, B := In2, C := Out} = Data,

    case eval({Command, In1, In2, Out}, Data) of
	halt ->
	    Data;
	Data1 ->
	   run_program(Pc+4, Data1)
    end.

eval({99, _In1, _In2, _Out}, _Data) ->
    halt;
eval({1, In1, In2, Out}, Data ) ->
    #{In1 := Arg1, In2:= Arg2} = Data,
    Data#{Out => Arg1 + Arg2};
eval({2, In1, In2, Out}, Data) ->
    #{In1 := Arg1, In2:= Arg2} = Data,
    Data#{Out => Arg1 * Arg2}.
 




