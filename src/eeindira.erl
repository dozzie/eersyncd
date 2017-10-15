%%%---------------------------------------------------------------------------
%%% @doc
%%%   Planned Indira interface.
%%% @end
%%%---------------------------------------------------------------------------

-module(eeindira).

-export([set_env/2, default_env/1]).
-export([reload/0, set_reload/1]).
-export([wait_for_start/1, is_started/1]).
-export([format_stacktrace/1]).

-export_type([]).

%%%---------------------------------------------------------------------------
%%% types

-define(RELOAD_MUTEX_NAME, '$indira_reload').

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

%%%---------------------------------------------------------------------------

%% @doc Call reload function.
%%
%% @see set_reload/1
%% @see format_stacktrace/1

-spec reload() ->
  term() | {error, reload_not_set | reload_in_progress}.

reload() ->
  case application:get_env(indira, reload_function) of
    {ok, {Mod, Fun, Args}} ->
      case reload_mutex_lock() of
        true ->
          try
            apply(Mod, Fun, Args)
          after
            reload_mutex_unlock()
          end;
        false ->
          {error, reload_in_progress}
      end;
    undefined ->
      {error, reload_not_set}
  end.

%% @doc Try acquiring the mutex that guards reloading procedure.

-spec reload_mutex_lock() ->
  boolean().

reload_mutex_lock() ->
  Self = self(),
  Pid = spawn(fun() ->
    try register(?RELOAD_MUTEX_NAME, self()) of
      true ->
        Self ! {?RELOAD_MUTEX_NAME, self(), true},
        receive
          {unlock, Self} -> ok
        end
    catch
      error:badarg ->
        Self ! {?RELOAD_MUTEX_NAME, self(), false}
    end
  end),
  receive
    {?RELOAD_MUTEX_NAME, Pid, Result} -> Result
  end.

%% @doc Release the mutex that guards reloading procedure.

reload_mutex_unlock() ->
  ?RELOAD_MUTEX_NAME ! {unlock, self()}.

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

%% @doc Format stacktrace returned by {@link erlang:get_stacktrace/0} as
%%   a JSON-serializable object.
%%
%% @see indira_json:encode/1

-spec format_stacktrace(StackTrace :: [Entry]) ->
  [indira_json:jhash()]
  when Entry :: {Module, Function, Args, Location},
       Module :: atom(),
       Function :: atom(),
       Args :: non_neg_integer() | [term()],
       Location :: [{atom(), term()}].

format_stacktrace(StackTrace) ->
  _Result = [
    [{function, format_function(M, F, A)} | format_location(L)] ||
    {M, F, A, L} <- StackTrace
  ].

%% @doc Format function name as a string (binary).

-spec format_function(atom(), atom(), integer() | list()) ->
  binary().

format_function(Mod, Fun, Args) when is_list(Args) ->
  format_function(Mod, Fun, length(Args));
format_function(Mod, Fun, Arity) when is_integer(Arity) ->
  iolist_to_binary([
    atom_to_list(Mod), $:,
    atom_to_list(Fun), $/,
    integer_to_list(Arity)
  ]).

%% @doc Format location as a JSON-serializable hash.

-spec format_location([{atom(), term()}]) ->
  indira_json:jhash().

format_location([] = _Location) ->
  [{}];
format_location(Location) ->
  [format_location_element(E) || E <- Location].

%% @doc Workhorse for {@link format_location/1}.

-spec format_location_element({atom(), term()}) ->
  {indira_json:jstring(), indira_json:jscalar()}.

format_location_element({file, File}) when is_list(File) ->
  {file, list_to_binary(File)};
format_location_element({line, Line}) when is_integer(Line) ->
  {line, Line};
format_location_element({Name, Value}) ->
  {Name, iolist_to_binary(io_lib:print(Value, 1, 16#FFFFFFFF, -1))}.

%%%---------------------------------------------------------------------------

%% @doc Wait until the specified supervisor (and, transitively, all its
%%   children) starts properly.
%%
%%   On supervisor's start error, `error' is returned.

-spec wait_for_start(pid() | atom()) ->
  ok | error.

wait_for_start(Supervisor) ->
  try supervisor:which_children(Supervisor) of
    _ -> ok
  catch
    _:_ -> error
  end.

%% @doc Check if an application is started.

-spec is_started(atom()) ->
  boolean().

is_started(App) ->
  % `{AppName :: atom(), Desc :: string(), Version :: string()}' or `false';
  % only non-false when the application started successfully (it's still
  % `false' during boot time)
  AppEntry = lists:keyfind(App, 1, application:which_applications()),
  AppEntry /= false.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
