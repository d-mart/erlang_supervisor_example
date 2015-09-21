-module(foo_app).

-export([start/0]).

start() ->
  % Start the supervisor
  {ok, Sup} = foo_sup:start_link(),

  % Get pid of worker from supervisor (we cheat and know there's just 1)
  [{foo, Worker1, worker, _}] = supervisor:which_children(Sup),

  % Make the worker do something
  gen_server:call(Worker1, {echo, "Hello, I'm worker 1"}),

  % The worker runs into a problem and dies.
  % (Note: gen_server:call brings the trouble back home and kills us as well)
  gen_server:cast(Worker1, die),

  % Wait while supervisor spins up another worker and get its pid
  timer:sleep(100),
  [{foo, Worker2, worker, _}] = supervisor:which_children(Sup),

  % Make the new worker process do something
  gen_server:call(Worker2, {echo, "Hello, I'm worker 2"}),

  % You know it
  success.
