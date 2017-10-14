%%%---------------------------------------------------------------------------
%%% @doc
%%% @end
%%%---------------------------------------------------------------------------

-module(eersyncd_sup).

-behaviour(supervisor).

%% supervision tree API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%---------------------------------------------------------------------------
%%% supervision tree API
%%%---------------------------------------------------------------------------

%% @private
%% @doc Start the supervisor process.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%---------------------------------------------------------------------------
%%% supervisor callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Initialize supervisor.

init([] = _Args) ->
  {ok, LogHandlers} = application:get_env(log_handlers),
  Strategy = {one_for_one, 5, 10},
  Children = [
    {eersyncd_log, {eersyncd_log, start_link, [LogHandlers]},
      permanent, 1000, worker, [eersyncd_log]},
    {eersyncd_tcp_sup, {eersyncd_tcp_sup, start_link, []},
      permanent, 1000, supervisor, [eersyncd_tcp_sup]}
  ],
  {ok, {Strategy, Children}}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
