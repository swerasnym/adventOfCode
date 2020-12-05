-module(day5).

-export([run/2]).


run(Star, File) ->
    Data = read(File),

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
   hd( lists:reverse(lists:sort(Data))).

star2(Data) ->
    find(    lists:sort(Data)).

read(File) ->
    {ok, Bin} = file:read_file(File),
    
    [to_int(Ticket) || Ticket <- 
    string:split(string:trim(binary_to_list(Bin)), "\n", all)].





to_int(Ticket) ->
    Substitute1 = string:replace(Ticket, "F", "0", all),
    Substitute2 = string:replace(Substitute1, "B", "1", all),
    Substitute3 = string:replace(Substitute2, "L", "0", all),
    Substitute4 = string:replace(Substitute3, "R", "1", all),
    Num = lists:flatten(Substitute4),
    list_to_integer(Num, 2).



find([A,B|_Rest]) when A+1 /= B ->
    A + 1;
find([_|Rest]) ->
    find(Rest).






