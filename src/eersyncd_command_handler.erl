%%%---------------------------------------------------------------------------
%%% @doc
%%%   Administrative commands handler for Indira.
%%%
%%% @see eersyncd_cli_handler
%%% @end
%%%---------------------------------------------------------------------------

-module(eersyncd_command_handler).

-behaviour(gen_indira_command).

%% gen_indira_command callbacks
-export([handle_command/2]).

%% interface for eersyncd_cli_handler
-export([format_request/1, parse_reply/2, hardcoded_reply/1]).

%%%---------------------------------------------------------------------------
%%% gen_indira_command callbacks
%%%---------------------------------------------------------------------------

%% @private
%% @doc Handle administrative commands sent to the daemon.

handle_command([{<<"command">>, <<"status">>}, {<<"wait">>, false}] = _Command,
               _Args) ->
  case indira_app:is_started(eersyncd) of
    true  -> [{result, running}];
    false -> [{result, stopped}]
  end;
handle_command([{<<"command">>, <<"status">>}, {<<"wait">>, true}] = _Command,
               _Args) ->
  case indira_app:wait_for_start(eersyncd_sup) of
    ok    -> [{result, running}];
    error -> [{result, stopped}]
  end;

handle_command([{<<"command">>, <<"stop">>}] = _Command, _Args) ->
  log_info(stop, "stopping eersyncd daemon", []),
  init:stop(),
  [{result, ok}, {pid, list_to_binary(os:getpid())}];

handle_command([{<<"command">>, <<"reload_config">>}] = _Command, _Args) ->
  log_info(reload, "reloading configuration", []),
  try indira_app:reload() of
    ok ->
      [{result, ok}];
    {error, reload_not_set} ->
      [{result, error}, {message, <<"not configured from file">>}];
    {error, reload_in_progress} ->
      log_info(reload, "another reload in progress", []),
      [{result, error}, {message, <<"reload command already in progress">>}];
    {error, Errors} ->
      % TODO: change the formatting of the errors
      log_error(reload, "reload errors", [{errors, {term, Errors}}]),
      % TODO: explain the errors (`eersyncd_cli_handler:format_error()')
      [{result, error}, {message, <<"reload errors (TODO: describe)">>}]
  catch
    Type:Error ->
      % XXX: this crash should never happen and is a programming error
      log_error(reload, "reload crash", [
        {crash, Type}, {error, {term, Error}},
        {stack_trace, indira_app:format_stacktrace(erlang:get_stacktrace())}
      ]),
      [{result, error},
        {message, <<"reload function crashed, check logs for details">>}]
  end;

handle_command([{<<"command">>, <<"reopen_logs">>}] = _Command, _Args) ->
  log_info(reopen_logs, "reopening log files", []),
  case application:get_env(eersyncd, error_logger_file) of
    {ok, Path} ->
      case indira_disk_h:reopen(error_logger, Path) of
        ok ->
          [{result, ok}];
        {error, Reason} ->
          [{result, error},
            {message, iolist_to_binary(indira_disk_h:format_error(Reason))}]
      end;
    undefined ->
      [{result, ok}]
  end;

handle_command([{<<"command">>, <<"dist_start">>}] = _Command, _Args) ->
  log_info(dist_start, "starting Erlang networking", []),
  case indira_app:distributed_start() of
    ok ->
      [{result, ok}];
    {error, Reason} ->
      log_error(dist_start, "can't setup Erlang networking",
                [{error, {term, Reason}}]),
      [{result, error}, {message, <<"Erlang networking error">>}]
  end;

handle_command([{<<"command">>, <<"dist_stop">>}] = _Command, _Args) ->
  log_info(dist_stop, "stopping Erlang networking", []),
  case indira_app:distributed_stop() of
    ok ->
      [{result, ok}];
    {error, Reason} ->
      log_error(dist_stop, "can't shutdown Erlang networking",
                [{error, {term, Reason}}]),
      [{result, error}, {message, <<"Erlang networking error">>}]
  end;

handle_command(Command, _Args) ->
  % `Command' is a structure coming from JSON, so it's safe to log it as it is
  eersyncd_log:warn(unknown_command, "unknown command", [{command, Command}]),
  [{result, error}, {message, <<"unrecognized command">>}].

%%%---------------------------------------------------------------------------
%%% interface for eersyncd_cli_handler
%%%---------------------------------------------------------------------------

%% @doc Encode administrative command as a serializable structure.

-spec format_request(gen_indira_cli:command()) ->
  gen_indira_cli:request().

format_request(status        = Command) -> [{command, Command}, {wait, false}];
format_request(status_wait   =_Command) -> [{command, status}, {wait, true}];
format_request(stop          = Command) -> [{command, Command}];
format_request(reload_config = Command) -> [{command, Command}];
format_request(reopen_logs   = Command) -> [{command, Command}];
format_request(dist_start    = Command) -> [{command, Command}];
format_request(dist_stop     = Command) -> [{command, Command}].

%% @doc Decode a reply to an administrative command.

-spec parse_reply(gen_indira_cli:reply(), gen_indira_cli:command()) ->
  term().

%% generic replies
parse_reply([{<<"result">>, <<"ok">>}] = _Reply, _Command) ->
  ok;
parse_reply([{<<"result">>, <<"todo">>}] = _Reply, _Command) ->
  {error, 'TODO'};
parse_reply([{<<"message">>, Message}, {<<"result">>, <<"error">>}] = _Reply,
            _Command) ->
  {error, Message};

parse_reply([{<<"pid">>, Pid}, {<<"result">>, <<"ok">>}] = _Reply,
            stop = _Command) ->
  {ok, Pid};

parse_reply([{<<"result">>, <<"running">>}]  = _Reply, status = _Command) ->
  running;
parse_reply([{<<"result">>, <<"stopped">>}]  = _Reply, status = _Command) ->
  stopped;

%% unrecognized reply
parse_reply(_Reply, _Command) ->
  {error, unrecognized_reply}.

%% @doc Artificial replies for `eersyncd_cli_handler' when no reply was
%%   received.

-spec hardcoded_reply(generic_ok | daemon_stopped) ->
  gen_indira_cli:reply().

hardcoded_reply(daemon_stopped = _Event) -> [{<<"result">>, <<"stopped">>}];
hardcoded_reply(generic_ok     = _Event) -> [{<<"result">>, <<"ok">>}].

%%%---------------------------------------------------------------------------

-spec log_info(atom(), eersyncd_log:event_message(),
               eersyncd_log:event_info()) ->
  ok.

log_info(Command, Message, Context) ->
  eersyncd_log:info(command, Message, Context ++ [{command, {term, Command}}]).

-spec log_error(atom(), eersyncd_log:event_message(),
                eersyncd_log:event_info()) ->
  ok.

log_error(Command, Message, Context) ->
  eersyncd_log:warn(command, Message, Context ++ [{command, {term, Command}}]).

%-spec format_term(any()) ->
%  binary().
%
%format_term(Term) when is_binary(Term) ->
%  Term;
%format_term(Term) ->
%  iolist_to_binary(io_lib:print(Term, 1, 16#ffffffff, -1)).

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
