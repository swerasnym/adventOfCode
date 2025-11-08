-module(aoc_web).

-behaviour(gen_server).

-doc "Provide an cached API towards https://adventofcode.com".
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([get_input_path/2, get_input_path/3, base_url/0]).
-export([get_aoc_page/2, get_aoc_page/3]).
-export([run/0]).
-export([get_problem_path/2, get_problem_path/3, check_date/2]).
-export([get_answers/2]).
-export([get_answer/3]).
-export([get_input_path/1]).

-define(BASE_URL, "https://adventofcode.com/").
-define(PACING, 5000).
-define(RELEASE_TIME, {5, 0, 0}).
-define(REPO, "https://github.com/swerasnym/adventOfCode").
-define(CONTACT, "https://github.com/swerasnym/adventOfCode/issues/new").

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
            Result = store_remote_aoc_page(Url, Path),
            [gen_statem:reply(From, Result) || From <- maps:get(UP, Requesters, [])],

            erlang:send_after(?PACING, self(), process_downloads),

            {noreply, State#state{queue = Queue1, requesters = maps:remove(UP, Requesters)}}
    end.

session_cookie() ->
    case application:get_env(aoc, session_id) of
        undefined ->
            {error, "session_id is not configured!"};
        {ok, ""} ->
            {error, "session_id is empty!"};
        {ok, ["session=" | _]} = Session ->
            {ok, {"Cookie", Session}};
        {ok, Session} ->
            {ok, {"Cookie", "session=" ++ Session}}
    end.

user_agent() ->
    {"User-Agent", ?REPO ++ " contact: " ++ ?CONTACT}.

ensure_paths() ->
    [ok = filelib:ensure_path(get_dir(D)) || D <- [inputs, problems]],
    ok.

base_path() ->
    {ok, Path} = application:get_env(aoc, base_path),
    filename:absname(Path).

get_dir(inputs) ->
    filename:join(base_path(), "inputs");
get_dir(problems) ->
    filename:join(base_path(), "problems").

get_input_path({Year, Day}) ->
    get_input_path(Year, Day, cached).

get_input_path(Year, Day) ->
    get_input_path(Year, Day, cached).

get_input_path(Year, Day, Type) ->
    maybe
        ok ?= check_date(Year, Day),
        Dir = filename:join(get_dir(inputs), integer_to_list(Year)),
        File = "day" ++ integer_to_list(Day) ++ ".txt",
        Path = filename:join(Dir, File),
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
        Dir = filename:join(get_dir(problems), integer_to_list(Year)),
        File = "day" ++ integer_to_list(Day) ++ ".html",
        Path = filename:join(Dir, File),
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
                lists:flatten(
                    io_lib:format("Problem ~p of year ~p is not released yet!", [Day, Year])
                )}
    end;
check_date(Year, Day) ->
    {error, lists:flatten(io_lib:format("Problem ~p of year ~p will never exist!", [Day, Year]))}.

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

store_remote_aoc_page(LocalUrl, Path) ->
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
    case session_cookie() of
        {ok, SessionCookie} ->
            get_url(Url, [SessionCookie]);
        {error, Error} ->
            {error, Error}
    end;
get_url(Url, Options) ->
    io:format("Downloading: ~s~n", [Url]),
    case httpc:request(get, {Url, [user_agent() | Options]}, [], [{full_result, false}]) of
        {ok, {200, Page}} ->
            {ok, Page};
        {ok, {Code, Error}} when is_number(Code), is_list(Error) ->
            {error, "Url: " ++ Url ++ " Code: " ++ integer_to_list(Code) ++ " Error: " ++ Error};
        {error, _} = Error ->
            {error, Error}
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
        [_ | AnswerBlocks] ?= binary:split(File, <<"<p>Your puzzle answer was <code>">>, [global]),
        {ok, Answers} ?= extract_answers(AnswerBlocks, []),
        Answers
    else
        Error ->
            io:format("No answer due to ~p.", [Error]),
            []
    end.

extract_answers([], Answers) ->
    {ok, lists:reverse(Answers)};
extract_answers([Line | Rest], Answers) ->
    case binary:split(Line, [<<"</code>">>]) of
        [Answer, _] ->
            extract_answers(Rest, [binary_to_answer(Answer) | Answers]);
        _ ->
            {error, {failed_to_split, Line}}
    end.

binary_to_answer(B) ->
    Functions = [
        fun erlang:binary_to_integer/1,
        fun erlang:binary_to_list/1
    ],
    {try_fun(Functions, B), erlang:binary_to_list(B)}.

try_fun([Fun | Rest], B) ->
    try
        Fun(B)
    catch
        _:_ ->
            try_fun(Rest, B)
    end.
