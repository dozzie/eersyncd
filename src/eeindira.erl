%%%---------------------------------------------------------------------------
%%% @doc
%%%   Planned Indira interface.
%%% @end
%%%---------------------------------------------------------------------------

-module(eeindira).

-export([set_env/2, default_env/1]).
-export([reload/0, set_reload/1]).

-export_type([]).

%%%---------------------------------------------------------------------------
%%% types

%%%---------------------------------------------------------------------------
%%% public interface
%%%---------------------------------------------------------------------------

%% @doc Synchronize application's environment to the specified values.

-spec set_env(atom(), Environment :: [Param]) ->
  ok | {error, Reason}
  when Param :: {Name :: atom(), Value :: term()},
       Reason :: bad_name | bad_app | file:posix().

set_env(App, Environment) ->
  case default_env(App) of
    {ok, DefaultEnv} ->
      application:load(App),
      env_converge(App, Environment ++ DefaultEnv);
    {error, Reason} ->
      {error, Reason}
  end.

%% @doc Load application's default parameters.

-spec default_env(atom()) ->
  {ok, Environment :: [Param]} | {error, Reason}
  when Param :: {Name :: atom(), Value :: term()},
       Reason :: bad_name | bad_app | file:posix().

default_env(App) when is_atom(App) ->
  case code:lib_dir(App, ebin) of
    {error, bad_name} ->
      {error, bad_name};
    Ebin ->
      case file:consult(filename:join(Ebin, atom_to_list(App) ++ ".app")) of
        {ok, [{application, App, Spec}]} ->
          {ok, proplists:get_value(env, Spec, [])};
        {ok, _} ->
          % the file contents are of unexpected format
          {error, bad_app};
        {error, Reason} when is_tuple(Reason) ->
          % *.app file parse error
          {error, bad_app};
        {error, Reason} ->
          % most probably read error (eperm or similar)
          {error, Reason}
      end
  end.

%% @doc Update actual app environment to specified one, adding and deleting
%%   keys as necessary.

env_converge(App, Environment) ->
  % environment is a proplist: the earlier keys overwrite the later ones
  EnvDict = lists:foldr(
    fun({Name, Value}, Acc) -> dict:store(Name, Value, Acc) end,
    dict:new(),
    Environment
  ),
  lists:foreach(
    fun(Name) -> application:unset_env(App, Name) end,
    [N || {N,_} <- application:get_all_env(App), not dict:is_key(N, EnvDict)]
  ),
  dict:fold(
    fun(Name, Value, _Acc) -> application:set_env(App, Name, Value) end,
    [],
    EnvDict
  ),
  ok.

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
