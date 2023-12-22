-module(aoc_web).
-feature(maybe_expr, enable).

-behaviour(gen_server).

%% @doc Provide an cached API towards https://adventofcode.com
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_input_path/2, get_input_path/3, base_url/0]).
-export([get_aoc_page/2, get_aoc_page/3]).
-export([run/0]).
-export([get_problem_path/2, get_problem_path/3, check_date/2]).
-export([get_answers/2]).
-export([get_answer/3]).

-define(BASE_URL, "https://adventofcode.com/").
-define(PACING, 5000).
-define(RELEASE_TIME, {5, 0, 0}).

-record(state, {
    queue = queue:new() :: queue:queue(),
    requesters = #{} :: map(),
    processing = false :: boolean()
}).

run() ->
    start_link(),
    get_input_path(2015, 20),
    get_input_path(2022, 21),
    get_input_path(2022, 22),
    get_problem_path(2023, 12).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, nothing, []).

%% Returns the internal state.
init(nothing) ->
    {ok, _} = application:ensure_all_started([inets, ssl]),
    ok = ensure_paths(),
    io:format("Started!~n"),
    {ok, #state{}}.

handle_call(
    {download, UrlPath},
    From,
    #state{
        queue = Queue,
        requesters = Requesters,
        processing = Processing
    } = State
) ->
    % io:format("call ~p ~n", [State]),
    case maps:get(UrlPath, Requesters, []) of
        [] ->
            Queue1 = queue:in(UrlPath, Queue),
            inform_download(Processing),
            {noreply, State#state{
                queue = Queue1,
                requesters = Requesters#{UrlPath => [From]},
                processing = true
            }};
        OlderRequests ->
            {noreply, State#state{requesters = Requesters#{UrlPath := [From | OlderRequests]}}}
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
    #state{
        queue = Queue,
        requesters = Requesters
    } = State
) ->
    %% io:format("process_downloads ~p ~n", [State]),
    case queue:out(Queue) of
        {empty, _} ->
            {noreply, State#state{processing = false}};
        {{value, {Url, no_cache} = UP}, Queue1} ->
            Result = get_url(?BASE_URL ++ Url, [cookie]),
            [gen_statem:reply(From, Result) || From <- maps:get(UP, Requesters, [])],
            erlang:send_after(?PACING, self(), process_downloads),
            {noreply, State#state{queue = Queue1, requesters = maps:remove(UP, Requesters)}};
        {{value, {Url, Path} = UP}, Queue1} ->
            Result = store_remore_aoc_page(Url, Path),
            [gen_statem:reply(From, Result) || From <- maps:get(UP, Requesters, [])],

            erlang:send_after(?PACING, self(), process_downloads),

            {noreply, State#state{queue = Queue1, requesters = maps:remove(UP, Requesters)}}
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
    maybe
        ok ?= check_date(Year, Day),
        Dir = get_dir(inputs) ++ integer_to_list(Year),
        File = "day" ++ integer_to_list(Day) ++ ".txt",
        Path = Dir ++ "/" ++ File,
        LocalUrl = integer_to_list(Year) ++ "/day/" ++ integer_to_list(Day) ++ "/input",
        {ok, _InputPath} ?= get_aoc_page(LocalUrl, Path, Type)
    else
        {error, _} = Error ->
            Error;
        Error ->
            {error, Error}
    end.

get_problem_path(Year, Day) ->
    get_problem_path(Year, Day, cached).

get_problem_path(Year, Day, Type) ->
    maybe
        ok ?= check_date(Year, Day),
        Dir = get_dir(problems) ++ integer_to_list(Year),
        File = "day" ++ integer_to_list(Day) ++ ".html",
        Path = Dir ++ "/" ++ File,
        LocalUrl = integer_to_list(Year) ++ "/day/" ++ integer_to_list(Day),
        {ok, _Path} ?= get_aoc_page(LocalUrl, Path, Type)
    else
        {error, _} = Error ->
            Error;
        Error ->
            {error, Error}
    end.

check_date(Year, Day) when Year >= 2015, Day >= 1, Day =< 25 ->
    case calendar:universal_time() > {{Year, 12, Day}, ?RELEASE_TIME} of
        true ->
            ok;
        false ->
            {error,
                lists:flatten(io_lib:format("Problem ~p for ~p is not released yet!", [Day, Year]))}
    end;
check_date(Year, Day) ->
    {error, lists:flatten(io_lib:format("Problem ~p for ~p will never exist!", [Day, Year]))}.

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
        {ok, Page} ?= get_url(?BASE_URL ++ LocalUrl, [cookie]),
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

get_url(Url, [cookie]) ->
    case session_coockie() of
        {ok, SessionCoockie} ->
            get_url(Url, [SessionCoockie]);
        {error, Error} ->
            {error, Error}
    end;
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

get_answer(Year, Day, star1) ->
    case get_answers(Year, Day) of
        [] ->
            unknown;
        [Answer | _] ->
            Answer
    end;
get_answer(Year, Day, star2) ->
    case get_answers(Year, Day) of
        [_, Answer | _] ->
            Answer;
        _ ->
            unknown
    end;
get_answer(_Year, _Day, _) ->
    unknown.

get_answers(Year, Day) ->
    maybe
        {ok, Page} ?= aoc_web:get_problem_path(Year, Day),
        {ok, File} ?= file:read_file(Page),
        {ok, Tokens, []} ?= htmerl:sax(File),
        find_answers(Tokens, [])
    else
        Error ->
            io:format("No answer due to ~p.", [Error]),
            []
    end.

find_answers([], Answers) ->
    lists:reverse(Answers);
find_answers([{characters, <<"Your puzzle answer was">>}, _, {characters, Answer} | Rest], Answers) ->
    find_answers(Rest, [binary_to_answer(Answer) | Answers]);
find_answers([_ | Rest], Answers) ->
    find_answers(Rest, Answers).

binary_to_answer(B) ->
    Funs = [
        fun erlang:binary_to_float/1,
        fun erlang:binary_to_integer/1,
        fun erlang:binary_to_list/1
    ],
    try_fun(Funs, B).

try_fun([Fun | Rest], B) ->
    try
        Fun(B)
    catch
        _:_ ->
            try_fun(Rest, B)
    end.
