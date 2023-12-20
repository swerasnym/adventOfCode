-module(aoc_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5,
        auto_shutdown => never
    },
    ChildSpecs = [
        #{
            id => aoc_web,
            start => {aoc_web, start_link, []},
            restart => permanent,
            shutdown => brutal_kill,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
