-module(foo_sup).

-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

%% supervision child spec
%%
-define(SUPERVISEE, foo).
-define(CHILD(M, Type), {M, {M, start_link, []}, permanent, 1000, Type, [M]}).


start_link() ->
  supervisor:start_link(?MODULE, []).


init([]) ->
  ChildSpec  = ?CHILD(?SUPERVISEE, worker),
  StartSpecs = {{one_for_one, 5, 10}, [ChildSpec]},
  {ok, StartSpecs}.
