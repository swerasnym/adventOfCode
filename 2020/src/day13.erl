-module(day13).

-export([run/2]).

-import(lists, [zip/2, unzip/1, foldl/3, sum/1]).
-export([egcd/2, mod/2, mod_inv/2, chinese_remainder/1]).
 
egcd(_, 0) -> {1, 0};
egcd(A, B) ->
    {S, T} = egcd(B, A rem B),
    {T, S - (A div B)*T}.
 
mod_inv(A, B) ->
    {X, Y} = egcd(A, B),
    if
        A*X + B*Y =:= 1 -> X;
        true -> undefined
    end.
 
mod(A, M) ->
    X = A rem M,
    if
        X < 0 -> X + M;
        true -> X
    end.
 
calc_inverses([], []) -> [];
calc_inverses([N | Ns], [M | Ms]) ->
    case mod_inv(N, M) of
        undefined -> undefined;
        Inv -> [Inv | calc_inverses(Ns, Ms)]
    end.
 
chinese_remainder(Congruences) ->
    {Residues, Modulii} = unzip(Congruences),
    ModPI = foldl(fun(A, B) -> A*B end, 1, Modulii),
    CRT_Modulii = [ModPI div M || M <- Modulii],
    case calc_inverses(CRT_Modulii, Modulii) of
        undefined -> undefined;
        Inverses ->
            Solution = sum([A*B || {A,B} <- zip(CRT_Modulii,
                                    [A*B || {A,B} <- zip(Residues, Inverses)])]),
            mod(Solution, ModPI)
    end.


run(Star, File) ->

    Time = 1000655,
        Buses = [17,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,571,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,x,23,x,x,x,x,x,29,x,401,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,19],
    



  Data = {Time,Buses},
    

  % Data = {939, [7,13,x,x,59,x,31,19]},
 %  Data = {939, [17,x,13,19]},


   %%  Data = read(File),

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

star1({Time, Buses}) ->
   {DepTime, Id} = lists:min([{dep_time(Id, Time), Id} || Id <- Buses, Id /= x  ]),
    Id* (DepTime - Time).

star2({_Time, Buses}) ->

    Reminders = [{-Rem,Id}|| {Id,Rem} <- lists:zip(Buses, lists:seq(0, length(Buses)-1)), Id /=x],
    X = chinese_remainder(Reminders),
    io:format("~p~n", [X]),
    [{X rem Id - Id, Er, Id} || {Er, Id} <- Reminders].


    

dep_time(Id, Time) ->
    case (Time div Id)*Id of
        Time ->
            Time;
        Other ->
            Other + Id
    end. 
    



%% read(File) ->
%%     {ok, Bin} = file:read_file(File),
%%     [
%%      action(Line)
%%      || Line
%%             <- string:split(
%%                  string:trim(binary_to_list(Bin)), "\n", all)].

