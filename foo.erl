-module(foo).

-behavior(gen_server).

% supervisor hooks
-export([start_link/0, stop/0]).

% gen_server API
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state, {
  everything = 42   :: integer(),
  happy      = true :: boolean()
}).

-define(LOG(Format, Args), io:format(Format ++ "~n", Args)).

-define(SELF_CALL_INTERVAL, 2000).
%%
%% supervisor hooks
%%
start_link() ->
  ?LOG("foo:start_link", []),
  gen_server:start_link(?MODULE, [], []).

stop() ->
  ?LOG("foo:stop", []),
  gen_server:stop(?MODULE).

%%%
%%% gen_server API
%%%
init(Args) ->
  ?LOG("~p started with args: ~p [Pid: ~p]", [?MODULE, Args, erlang:self()]),
  {ok, #state{}}.

handle_call(start, _Caller, State) ->
  ?LOG("Starting periodic self-call", []),
  schedule_next_call(),
  {reply, scheduled, State};
handle_call({echo, Thing}, _Caller, State) ->
  ?LOG("~p Echo: ~p", [erlang:self(), Thing]),
  {reply, echoed, State};
handle_call(die, _Caller, State) ->
  suicide(),
  {reply, never_see_this, State};
handle_call(Msg, Caller, State) ->
  ?LOG("Unhandled call from ~p: ~p", [Caller, Msg]),
  {reply, undefined, State}.

handle_cast(die, State) ->
  suicide(),
  {noreply, State};
handle_cast(Msg, State) ->
  ?LOG("Unhandled cast: ~p", [Msg]),
  {noreply, State}.

handle_info(self_call, State) ->
  schedule_next_call(),
  {noreply, State};
handle_info(Info, State) ->
  ?LOG("Info received: ~p", [Info]),
  {noreply, State}.

code_change(OldVersion, State, _Extra) ->
  ?LOG("Code change; upgrading from version ~p", [OldVersion]),
  {ok, State}.

terminate(Reason, _State) ->
  ?LOG("Terminating for reason: ~p", [Reason]),
  ok.


%%%
%%% internal/helper functions
%%%
suicide() ->
  ?LOG("Suicide by bad call", []),
  bad_module:non_existing_function_call_that_causes_crash().

schedule_next_call() ->
  ?LOG("~p Scheduling another self-call", [erlang:time()]),
  erlang:send_after(?SELF_CALL_INTERVAL, self(), self_call).
