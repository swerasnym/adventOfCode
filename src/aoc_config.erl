-module(aoc_config).
-feature(maybe_expr, enable).

-export([check/0]).
-export([init/0]).

init() ->
    case filelib:is_file("aoc.config") of
        false ->
            io:format("Creating aoc.config.~n"),
            {ok, _} = file:copy(default_config(), "aoc.config"),
            success;
        true ->
            io:format("aoc.config already exists in the repo root. Checking its contents...~n"),
            check()
    end.

check() ->
    case filelib:is_file("aoc.config") of
        false ->
            io:format("Error: No aoc.config in the repo root.~n"),
            error(check_failed);
        true ->
            case has_all_keys() of
                error ->
                    error(check_failed);
                Res ->
                    Res
            end
    end.

has_all_keys() ->
    maybe
        {ok, [Configured]} ?= file:consult("aoc.config"),
        case length(Configured) > 1 of
            true ->
                ExtraApps = [A || A <- proplists:get_keys(Configured), A /= aoc],
                io:format(
                    "Warning: aoc.config has configuration for the following extra apps: ~0p.~n", [
                        ExtraApps
                    ]
                );
            false ->
                ok
        end,
        ConfiguredEnv = proplists:get_value(aoc, Configured),
        [_ | _] ?= ConfiguredEnv,
        {ok, [[{aoc, ExpectedEnv}]]} = file:consult(default_config()),
        ConfiguredKeys = proplists:get_keys(ConfiguredEnv),
        ExpectedKeys = proplists:get_keys(ExpectedEnv),
        Extra = [K || K <- ConfiguredKeys, not lists:member(K, ExpectedKeys)],
        Missing = [K || K <- ExpectedKeys, not lists:member(K, ConfiguredKeys)],
        case {Extra, Missing} of
            {[], []} ->
                io:format("Success: aoc.config has the correct format!~n"),
                success;
            {Extra, []} ->
                io:format("Warning: aoc.config has the following extra keys: ~0p.~n", [Extra]),
                warning;
            {[], Missing} ->
                io:format("Error: aoc.config are missing the following keys: ~0p.~n", [Missing]),
                error;
            {Extra, Missing} ->
                io:format(
                    "Error: aoc.config are missing the following keys: ~0p~n"
                    "       and has the following extra keys: ~0p.~n",
                    [Missing, Extra]
                ),
                error
        end
    else
        {error, Error} ->
            io:format("Some error occurred: ~p", [Error]),
            error;
        {ok, _} ->
            io:format("Error: aoc.config are not a proper config file.~n"),
            error;
        undefined ->
            io:format("Error: aoc.config are missing config for the aoc app.~n"),
            error
    end.

default_config() ->
    filename:join(code:priv_dir(aoc), "aoc.default.config").
