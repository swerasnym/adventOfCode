-module(day6).
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
   All =  [begin 
              length(maps:to_list(count(lists:flatten(Group))))                       
           end|| Group <- Data],

    
   lists:sum(lists:flatten(All)).
star2(Data) ->
       All =  [begin 
           
                   Count = count(lists:flatten(Group)) ,
               
                   [K || {K,V} <- maps:to_list(Count), V == length(Group) ]
                                            
           end || Group <- Data],               
          length(lists:flatten(All)).

read(File) ->
    {ok, Bin} = file:read_file(File),
    
    [process_group(Group) || Group <- 
    string:split(string:trim(binary_to_list(Bin)), "\n\n", all)].


process_group(Group) ->
    string:split(string:trim(Group), "\n", all).
   
count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).





