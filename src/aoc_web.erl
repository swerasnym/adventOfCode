-module(aoc_web).
-feature(maybe_expr, enable).

-behaviour(gen_server).

%% @doc Provide an cached API towards https://adventofcode.com
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_input_path/2, get_input_path/3, base_url/0]).
-export([get_aoc_page/2, get_aoc_page/3]).
-export([run/0]).
-export([get_problem_path/2, get_problem_path/3]).

-define(BASE_URL, "https://adventofcode.com/").
-define(PACING, 5000).
-define(RELEASE_TIME, {6, 0, 0}).
run() ->
    start_link(),
    get_input_path(2015, 20),
    get_input_path(2022, 21),
    get_input_path(2022, 22),
    get_problem_path(2023, 11).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nothing, []).

%% Returns the internal state.
init(nothing) ->
    {ok, _} = application:ensure_all_started([inets, ssl]),
    ok = ensure_paths(),
    io:format("Started!~n"),
    {ok, #{
        queue => queue:new(),
        requesters => #{},
        processing => false
    }}.

handle_call(
    {download, UrlPath},
    From,
    #{
        queue := Queue,
        requesters := Requesters,
        processing := Processing
    } = State
) ->
    % io:format("call ~p ~n", [State]),
    case maps:get(UrlPath, Requesters, []) of
        [] ->
            Queue1 = queue:in(UrlPath, Queue),
            inform_download(Processing),
            {noreply, State#{
                queue := Queue1,
                requesters := Requesters#{UrlPath => [From]},
                processing := true
            }};
        OlderRequests ->
            {noreply, State#{requesters := Requesters#{UrlPath := [From | OlderRequests]}}}
    end.

inform_download(false) ->
    self() ! process_downloads,
    ok;
inform_download(true) ->
    ok.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(
    process_downloads,
    #{
        queue := Queue,
        requesters := Requesters
    } = State
) ->
    %% io:format("process_downloads ~p ~n", [State]),
    case queue:out(Queue) of
        {empty, _} ->
            {noreply, State#{processing := false}};
        {{value, {Url, Path} = UP}, Queue1} ->
            Result = store_remore_aoc_page(Url, Path),
            [gen_statem:reply(From, Result) || From <- maps:get(UP, Requesters, [])],

            timer:send_after(?PACING, process_downloads),

            {noreply, State#{queue := Queue1, requesters := maps:remove(UP, Requesters)}}
    end.

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
    application:get_env(aoc_web, storage_path, Home ++ "/aoc_storage/").

get_dir(inputs) ->
    base_dir() ++ "inputs/";
get_dir(problems) ->
    base_dir() ++ "problems/";
get_dir(results) ->
    base_dir() ++ "results/".

get_input_path(Year, Day) ->
    get_input_path(Year, Day, cached).

get_input_path(Year, Day, Type) ->
    ok = check_date(Year, Day),

    Dir = get_dir(inputs) ++ integer_to_list(Year),
    File = "day" ++ integer_to_list(Day) ++ ".txt",
    Path = Dir ++ "/" ++ File,
    LocalUrl = integer_to_list(Year) ++ "/day/" ++ integer_to_list(Day) ++ "/input",

    case get_aoc_page(LocalUrl, Path, Type) of
        {ok, InputPath} ->
            io:format("Input path: ~s~n", [InputPath]),
            InputPath;
        Error ->
            Error
    end.

get_problem_path(Year, Day) ->
    get_problem_path(Year, Day, cached).

get_problem_path(Year, Day, Type) ->
    ok = check_date(Year, Day),

    Dir = get_dir(problems) ++ integer_to_list(Year),
    File = "day" ++ integer_to_list(Day) ++ ".html",
    Path = Dir ++ "/" ++ File,
    LocalUrl = integer_to_list(Year) ++ "/day/" ++ integer_to_list(Day),

    case get_aoc_page(LocalUrl, Path, Type) of
        {ok, InputPath} ->
            io:format("Problem path: ~s~n", [InputPath]),
            InputPath;
        Error ->
            Error
    end.

check_date(Year, Day) when Year >= 2015, Day >= 1, Day =< 25 ->
    case calendar:local_time() > {{Year, 12, Day}, ?RELEASE_TIME} of
        true ->
            ok;
        false ->
            error("Problem is not realased yet!", [Year, Day])
    end;
check_date(Year, Day) ->
    error("No such problem will ever exist", [Year, Day]).

get_aoc_page(LocalUrl, Path) ->
    get_aoc_page(LocalUrl, Path, cached).

get_aoc_page(LocalUrl, Path, cached) ->
    case filelib:is_file(Path) of
        true ->
            {ok, Path};
        false ->
            get_aoc_page(LocalUrl, Path, remote)
    end;
get_aoc_page(LocalUrl, Path, remote) ->
    gen_server:call(?MODULE, {download, {LocalUrl, Path}}, infinity).

%% internal
%%
base_url() ->
    ?BASE_URL.

store_remore_aoc_page(LocalUrl, Path) ->
    maybe
        {ok, SessionCoockie} ?= session_coockie(),
        {ok, Page} ?= get_url(?BASE_URL ++ LocalUrl, [SessionCoockie]),
        ok = filelib:ensure_path(filename:dirname(Path)),
        case file:write_file(Path, Page) of
            ok ->
                {ok, Path};
            WriteError ->
                {error, {saving, Page, Path, WriteError}}
        end
    else
        {error, _} = Error ->
            Error;
        Error ->
            {error, Error}
    end.

get_url(Url, Options) ->
    io:format("Downloading: ~s~n", [Url]),
    case httpc:request(get, {Url, Options}, [], [{full_result, false}]) of
        {ok, {200, Page}} ->
            {ok, Page};
        {ok, {404, Error}} ->
            {error, {forbidden, Error}};
        {error, _} = Error ->
            Error
    end.
