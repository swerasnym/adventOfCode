-module(aoc_web).
-feature(maybe_expr, enable).

-behaviour(gen_server).

%% @doc Provide an cached API towards https://adventofcode.com
-export([start_link/0]).
% -export([alloc/0, free/1]).
% -export([init/1, handle_call/3, handle_cast/2]).

%% for test for now...
-compile([export_all, nowarn_export_all]).

-define(BASE_URL, "https://adventofcode.com/").

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, nothing, []).

%% Returns the internal state.
init(nothing) ->
    application:ensure_all_started([inets, ssl]),
    {ok, #{}}.

session_coockie() ->
    Path = iolist_to_binary([base_dir(), "SESSION"]),
    case filelib:is_file(Path) of
        true ->
            % {ok, Data} = file:read_file(Path),
            % tools:read_grid(File, Fun),
            case tools:read_string(Path) of
                ["session=" | _] = Session ->
                    {ok, {"Cookie", Session}};
                Session ->
                    {ok, {"Cookie", "session=" ++ Session}}
            end;
        false ->
            {error, {nofile, Path}}
    end.

ensure_paths() ->
    [ok = filelib:ensure_path(get_dir(D)) || D <- [inputs, problems, results]],
    ok.

base_dir() ->
    {ok, [[Home]]} = init:get_argument(home),
    application:get_env(aoc_web, storage_path, [Home, "/aoc_storage/"]).

get_dir(inputs) ->
    [base_dir(), "/inputs"];
get_dir(problems) ->
    [base_dir(), "/inputs"];
get_dir(results) ->
    [base_dir(), "/results"].

get_input(Year, Day) ->
    get_input(Year, Day, cached).

get_input(Year, Day, Type) when Year >= 2015, Day >= 1, Day =< 25 ->
    Dir = get_dir(inputs) ++ "/" ++ integer_to_list(Year),
    File = "day" ++ integer_to_list(Day) ++ ".txt",
    Path = Dir ++ "/" ++ File,
    LocalUrl = integer_to_list(Year) ++ "/day/" ++ integer_to_list(Day) ++ "/input",

    ok = filelib:ensure_path(Dir),
    case get_aoc_page(LocalUrl, Path, Type) of
        {ok, Input} ->
            Input;
        Error ->
            Error
    end.

get_aoc_page(LocalUrl, Path) ->
    get_aoc_page(LocalUrl, Path, cached).

get_aoc_page(LocalUrl, Path, cached) ->
    case filelib:is_file(Path) of
        true ->
            {ok, file:read_file(Path)};
        false ->
            get_aoc_page(LocalUrl, Path, remote)
    end;
get_aoc_page(LocalUrl, Path, remote) ->
    maybe
        {ok, SessionCoockie} ?= session_coockie(),
        {ok, Page} ?= get_url(?BASE_URL ++ LocalUrl, [SessionCoockie]),
        case file:write_file(Path, Page) of
            ok ->
                {ok, Page};
            WriteError ->
                {error, {saving, Page, Path, WriteError}}
        end
    else
        {error, _} = Error ->
            Error;
        Error ->
            {error, Error}
    end.

%% internal
get_url(Url, Options) ->
    io:format("Fetching: ~s ~p~n", [Url, Options]),
    case httpc:request(get, {Url, Options}, [], [{full_result, false}]) of
        {ok, {200, Page}} ->
            {ok, Page};
        {ok, {404, Error}} ->
            {error, {forbidden, Error}};
        {error, _} = Error ->
            Error
    end.
