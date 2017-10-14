%%%---------------------------------------------------------------------------
%%% @doc
%%%   Planned Indira interface.
%%% @end
%%%---------------------------------------------------------------------------

-module(eeindira).

-export([set_env/2]).
-export([reload/0, set_reload/1]).

-export_type([]).

%%%---------------------------------------------------------------------------
%%% types

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Synchronize application's environment to the specified values.

-spec set_env(atom(), Environment :: [Param]) ->
  ok | {error, bad_app | badarg}
  when Param :: {Name :: atom(), Value :: term()}.

set_env(App, Environment) ->
  case application:load(App) of
    ok ->
      env_converge(App, Environment);
    {error, {already_loaded, App}} ->
      env_converge(App, Environment);
    {error, _} ->
      {error, bad_app}
  end.

%% @doc Update actual app environment to specified one, adding and deleting
%%   keys as necessary.
%%
%% @todo Make this function smarter

env_converge(_App, [] = _Environment) ->
  ok;
env_converge(App, [{Name, Value} | Rest] = _Environment) ->
  application:set_env(App, Name, Value),
  env_converge(App, Rest).

%% @doc Call reload function.
%%
%% @see set_reload/1

-spec reload() ->
  term() | {error, reload_not_set}.

reload() ->
  case application:get_env(indira, reload_function) of
    {ok, {Mod, Fun, Args}} -> apply(Mod, Fun, Args);
    undefined -> {error, reload_not_set}
  end.

%% @doc Set config reload function.
%%
%% @todo Move this to {@link indira_app:indira_setup/1} options

-spec set_reload(MFA :: {Mod :: module(), Fun :: atom(), Args :: [term()]}) ->
  ok.

set_reload({Mod, Fun, Args} = MFA)
when is_atom(Mod), is_atom(Fun), is_list(Args) ->
  case application:load(indira) of
    ok -> ok;
    {error, {already_loaded, indira}} -> ok
  end,
  application:set_env(indira, reload_function, MFA).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
