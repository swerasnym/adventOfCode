-module(day2).

-export([run/2]).

%% --- Day 2: Password Philosophy ---
%%
%% Your flight departs in a few days from the coastal airport; the easiest way
%% down to the coast from here is via toboggan.
%%
%% The shopkeeper at the North Pole Toboggan Rental Shop is having a bad
%% day. "Something's wrong with our computers; we can't log in!" You ask if you
%% can take a look.
%%
%% Their password database seems to be a little corrupted: some of the passwords
%% wouldn't have been allowed by the Official Toboggan Corporate Policy that was
%% in effect when they were chosen.
%%
%% To try to debug the problem, they have created a list (your puzzle input) of
%% passwords (according to the corrupted database) and the corporate policy when
%% that password was set.
%%
%% For example, suppose you have the following list:
%%
%% 1-3 a: abcde
%% 1-3 b: cdefg
%% 2-9 c: ccccccccc
%%
%% Each line gives the password policy and then the password. The password
%% policy indicates the lowest and highest number of times a given letter must
%% appear for the password to be valid. For example, 1-3 a means that the
%% password must contain a at least 1 time and at most 3 times.
%%
%% In the above example, 2 passwords are valid. The middle password, cdefg, is
%% not; it contains no instances of b, but needs at least 1. The first and third
%% passwords are valid: they contain one a or nine c, both within the limits of
%% their respective policies.
%%
%% How many passwords are valid according to their policies?
%%
%% Your puzzle answer was 524.
%%
%% --- Part Two ---
%%
%% While it appears you validated the passwords correctly, they don't seem to be
%% what the Official Toboggan Corporate Authentication System is expecting.
%%
%% The shopkeeper suddenly realizes that he just accidentally explained the
%% password policy rules from his old job at the sled rental place down the
%% street! The Official Toboggan Corporate Policy actually works a little
%% differently.
%%
%% Each policy actually describes two positions in the password, where 1 means
%% the first character, 2 means the second character, and so on. (Be careful;
%% Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of
%% these positions must contain the given letter. Other occurrences of the
%% letter are irrelevant for the purposes of policy enforcement.
%%
%% Given the same example list from above:
%%
%%     1-3 a: abcde is valid: position 1 contains a and position 3 does not.
%%     1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
%%     2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
%%
%% How many passwords are valid according to the new interpretation of the
%% policies?
%%
%% Your puzzle answer was 485.
%%
%% Both parts of this puzzle are complete! They provide two gold stars: **

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
    length([Line || Line <- Data, check_pwd(Line) == ok]).

star2(Data) ->
    length([Line || Line <- Data, check_pwd2(Line) == ok]).

read_data(Device) ->
    read_data(Device, []).

read_data(Device, Acc) ->
    case io:fread(Device, [], "~d-~d ~c:~s") of
        eof ->
            lists:reverse(Acc);
        {ok, D} ->
            read_data(Device, [D | Acc]);
        {error, What} ->
            io:format("io:fread error: ~w~n", [What]),
            read_data(Device, Acc)
    end.

check_pwd([Min, Max, [Char], Password]) ->
    Len = length([P || P <- Password, P == Char]),
    if Len < Min ->
           nok;
       Len > Max ->
           nok;
       true ->
           ok
    end.

check_pwd2([Idx1, Idx2, [Char], Password]) ->
    Pos1 = lists:nth(Idx1, Password),
    Pos2 = lists:nth(Idx2, Password),

    if Pos1 == Char, Pos2 == Char ->
           nok;
       Pos1 == Char ->
           ok;
       Pos2 == Char ->
           ok;
       true ->
           nok
    end.
