%%%---------------------------------------------------------------------------
%%% @doc
%%%   Module that handles command line operations.
%%%   This includes parsing provided arguments and either starting the daemon
%%%   or sending it various administrative commands.
%%%
%%% @see eersyncd_command_handler
%%% @end
%%%---------------------------------------------------------------------------

-module(eersyncd_cli_handler).

-behaviour(gen_indira_cli).

%% interface for daemonizing script
-export([format_error/1]).
-export([help/1]).

%% gen_indira_cli callbacks
-export([parse_arguments/2]).
-export([handle_command/2, format_request/2, handle_reply/3]).

%% "unused function" warning silencer exports
-export([println/1, printerr/1, printerr/2]).

%%%---------------------------------------------------------------------------
%%% types {{{

-define(ADMIN_COMMAND_MODULE, eersyncd_command_handler).
% XXX: `status' and `stop' commands are bound to few specific errors this
% module returns; this can't be easily moved to a config/option
-define(ADMIN_SOCKET_TYPE, indira_unix).

-record(opts, {
  op :: start | status | stop | reload_config
      | dist_start | dist_stop,
  admin_socket :: file:filename(),
  options :: [{atom(), term()}],
  args :: [string()]
}).

%%% }}}
%%%---------------------------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% gen_indira_cli callbacks
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% parse_arguments() {{{

%% @private
%% @doc Parse command line arguments and decode opeartion from them.

parse_arguments(Args, [DefAdminSocket, DefConfig] = _Defaults) ->
  EmptyOptions = #opts{
    admin_socket = DefAdminSocket,
    args = [],
    options = [
      {config, DefConfig}
    ]
  },
  case indira_cli:folds(fun cli_opt/2, EmptyOptions, Args) of
    {ok, Options = #opts{op = start}}  -> {ok, start, Options};
    {ok, Options = #opts{op = status}} -> {ok, status, Options};
    {ok, Options = #opts{op = stop}}   -> {ok, stop, Options};

    {ok, _Options = #opts{op = undefined}} ->
      help;

    {ok, _Options = #opts{op = _, args = [_|_]}} ->
      {error, too_many_args};

    {ok, Options = #opts{op = Command, admin_socket = AdminSocket}} ->
      {send, {?ADMIN_SOCKET_TYPE, AdminSocket}, Command, Options};

    {error, {help, _Arg}} ->
      help;

    {error, {Reason, Arg}} ->
      {error, {Reason, Arg}}
  end.

%% }}}
%%----------------------------------------------------------
%% handle_command() {{{

%% @private
%% @doc Execute commands more complex than "request -> reply -> print".

handle_command(start = _Command,
               _Options = #opts{admin_socket = Socket, options = CLIOpts}) ->
  ConfigPath = proplists:get_value(config, CLIOpts),
  SASLApp = case proplists:get_bool(debug, CLIOpts) of
    true -> sasl;
    false -> undefined
  end,
  PidFile = proplists:get_value(pidfile, CLIOpts),
  case file:consult(ConfigPath) of
    {ok, Configs} ->
      ok = application:load(eersyncd),
      lists:foreach(
        fun({Name, Value}) -> application:set_env(eersyncd, Name, Value) end,
        proplists:get_value(eersyncd, Configs)
      ),
      indira_app:daemonize(eersyncd, [
        {listen, [{?ADMIN_SOCKET_TYPE, Socket}]},
        {command, {?ADMIN_COMMAND_MODULE, []}},
        {start_before, SASLApp},
        {pidfile, PidFile} |
        proplists:get_value(indira, Configs, [])
      ]);
    {error, Reason} ->
      {error, Reason}
  end;

handle_command(status = Command,
               Options = #opts{admin_socket = Socket, options = CLIOpts}) ->
  Timeout = proplists:get_value(timeout, CLIOpts, infinity),
  Opts = case proplists:get_bool(wait, CLIOpts) of
    true  = Wait -> [{timeout, Timeout}, retry];
    false = Wait -> [{timeout, Timeout}]
  end,
  {ok, Request} = format_request(Command, Options),
  case indira_cli:send_one_command(?ADMIN_SOCKET_TYPE, Socket, Request, Opts) of
    {ok, Reply} ->
      handle_reply(Reply, Command, Options);
    {error, Reason} when Reason == timeout, Wait;
                         Reason == econnrefused;
                         Reason == enoent ->
      % FIXME: what to do when a command got sent, but no reply was received?
      % (Reason == timeout)
      Reply = ?ADMIN_COMMAND_MODULE:hardcoded_reply(daemon_stopped),
      handle_reply(Reply, Command, Options);
    {error, Reason} ->
      {error, {send, Reason}} % mimic what `indira_cli:execute()' returns
  end;

handle_command(stop = Command,
               Options = #opts{admin_socket = Socket, options = CLIOpts}) ->
  Timeout = proplists:get_value(timeout, CLIOpts, infinity),
  Opts = [{timeout, Timeout}],
  {ok, Request} = format_request(Command, Options),
  case indira_cli:send_one_command(?ADMIN_SOCKET_TYPE, Socket, Request, Opts) of
    {ok, Reply} ->
      handle_reply(Reply, Command, Options);
    {error, Reason} when Reason == closed;
                         Reason == econnrefused;
                         Reason == enoent ->
      Reply = ?ADMIN_COMMAND_MODULE:hardcoded_reply(generic_ok),
      handle_reply(Reply, Command, Options);
    {error, Reason} ->
      {error, {send, Reason}} % mimic what `indira_cli:execute()' returns
  end.

%% }}}
%%----------------------------------------------------------
%% format_request() + handle_reply() {{{

%% @private
%% @doc Format a request to send to daemon.

format_request(status = _Command, _Options = #opts{options = CLIOpts}) ->
  Request = case proplists:get_bool(wait, CLIOpts) of
    true  -> ?ADMIN_COMMAND_MODULE:format_request(status_wait);
    false -> ?ADMIN_COMMAND_MODULE:format_request(status)
  end,
  {ok, Request};
format_request(Command, _Options) ->
  Request = ?ADMIN_COMMAND_MODULE:format_request(Command),
  {ok, Request}.

%% @private
%% @doc Handle a reply to a command sent to daemon.

handle_reply(Reply, status = Command, _Options) ->
  % `status' and `status_wait' have the same `Command' and replies
  case ?ADMIN_COMMAND_MODULE:parse_reply(Reply, Command) of
    running ->
      println("eersyncd is running"),
      ok;
    stopped ->
      println("eersyncd is stopped"),
      {error, 1};
    % for future changes in status detection
    Status ->
      {error, {unknown_status, Status}}
  end;

handle_reply(Reply, stop = Command, _Options = #opts{options = CLIOpts}) ->
  PrintPid = proplists:get_bool(print_pid, CLIOpts),
  case ?ADMIN_COMMAND_MODULE:parse_reply(Reply, Command) of
    {ok, Pid} when PrintPid -> println(Pid), ok;
    {ok, _Pid} when not PrintPid -> ok;
    ok -> ok;
    {error, Reason} -> {error, Reason}
  end;

handle_reply(Reply, reload_config = Command, _Options) ->
  case ?ADMIN_COMMAND_MODULE:parse_reply(Reply, Command) of
    ok ->
      ok;
    {error, Message} when is_binary(Message) ->
      printerr(["reload error: ", Message]),
      {error, 1};
    {error, Errors} when is_list(Errors) ->
      printerr("reload errors:"),
      lists:foreach(
        fun({Part, Error}) -> printerr(["  ", Part, ": ", Error]) end,
        Errors
      ),
      {error, 1}
  end;

handle_reply(Reply, Command, _Options) ->
  ?ADMIN_COMMAND_MODULE:parse_reply(Reply, Command).

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% interface for daemonizing script
%%%---------------------------------------------------------------------------

%%----------------------------------------------------------
%% format_error() {{{

%% @doc Convert an error to a printable form.

-spec format_error(term()) ->
  iolist().

format_error(Reason) when is_binary(Reason) ->
  Reason;
format_error(Reason) ->
  ["error message TODO; error: ", io_lib:print(Reason, 1, 16#ffffffff, -1)].

%% }}}
%%----------------------------------------------------------
%% help() {{{

%% @doc Return a printable help message.

-spec help(string()) ->
  iolist().

help(ScriptName) ->
  _Usage = [
    "Experimental rsync daemon.\n",
    "Controlling daemon:\n",
    "  ", ScriptName, " [--socket <path>] start [--debug] [--config <path>] [--pidfile <path>]\n",
    "  ", ScriptName, " [--socket <path>] status [--wait [--timeout <seconds>]]\n",
    "  ", ScriptName, " [--socket <path>] stop [--timeout <seconds>] [--print-pid]\n",
    "  ", ScriptName, " [--socket <path>] reload\n",
    "Distributed Erlang support:\n",
    "  ", ScriptName, " [--socket <path>] dist-erl-start\n",
    "  ", ScriptName, " [--socket <path>] dist-erl-stop\n",
    ""
  ].

%% }}}
%%----------------------------------------------------------

%%%---------------------------------------------------------------------------
%%% various helpers
%%%---------------------------------------------------------------------------

%% @doc Command line argument parser for {@link indira_cli:folds/3}.

cli_opt("-h" = _Arg, _Opts) ->
  {error, help};
cli_opt("--help" = _Arg, _Opts) ->
  {error, help};

cli_opt("--socket=" ++ Socket = _Arg, Opts) ->
  cli_opt(["--socket", Socket], Opts);
cli_opt("--socket" = _Arg, _Opts) ->
  {need, 1};
cli_opt(["--socket", Socket] = _Arg, Opts) ->
  _NewOpts = Opts#opts{admin_socket = Socket};

cli_opt("--pidfile=" ++ Path = _Arg, Opts) ->
  cli_opt(["--pidfile", Path], Opts);
cli_opt("--pidfile" = _Arg, _Opts) ->
  {need, 1};
cli_opt(["--pidfile", Path] = _Arg, Opts = #opts{options = CLIOpts}) ->
  _NewOpts = Opts#opts{options = [{pidfile, Path} | CLIOpts]};

cli_opt("--config=" ++ Path = _Arg, Opts) ->
  cli_opt(["--config", Path], Opts);
cli_opt("--config" = _Arg, _Opts) ->
  {need, 1};
cli_opt(["--config", Path] = _Arg, Opts = #opts{options = CLIOpts}) ->
  _NewOpts = Opts#opts{options = [{config, Path} | CLIOpts]};

cli_opt("--timeout=" ++ Timeout = _Arg, Opts) ->
  cli_opt(["--timeout", Timeout], Opts);
cli_opt("--timeout" = _Arg, _Opts) ->
  {need, 1};
cli_opt(["--timeout", Timeout] = _Arg, Opts = #opts{options = CLIOpts}) ->
  case make_integer(Timeout) of
    {ok, Seconds} when Seconds > 0 ->
      % NOTE: we need timeout in milliseconds
      _NewOpts = Opts#opts{options = [{timeout, Seconds * 1000} | CLIOpts]};
    _ ->
      {error, bad_timeout}
  end;

cli_opt("--debug" = _Arg, Opts = #opts{options = CLIOpts}) ->
  _NewOpts = Opts#opts{options = [{debug, true} | CLIOpts]};

cli_opt("--wait" = _Arg, Opts = #opts{options = CLIOpts}) ->
  _NewOpts = Opts#opts{options = [{wait, true} | CLIOpts]};

cli_opt("--print-pid" = _Arg, Opts = #opts{options = CLIOpts}) ->
  _NewOpts = Opts#opts{options = [{print_pid, true} | CLIOpts]};

cli_opt("-" ++ _ = _Arg, _Opts) ->
  {error, bad_option};

cli_opt(Arg, Opts = #opts{op = Op, args = OpArgs}) when Op /= undefined ->
  % `++' operator is a little costly, but considering how many arguments there
  % will be, the time it all takes will drown in Erlang startup
  _NewOpts = Opts#opts{args = OpArgs ++ [Arg]};

cli_opt(Arg, Opts = #opts{op = undefined}) ->
  case Arg of
    "start"  -> Opts#opts{op = start};
    "status" -> Opts#opts{op = status};
    "stop"   -> Opts#opts{op = stop};
    "reload" -> Opts#opts{op = reload_config};
    "dist-erl-start" -> Opts#opts{op = dist_start};
    "dist-erl-stop"  -> Opts#opts{op = dist_stop};
    _ -> {error, bad_command}
  end.

%%%---------------------------------------------------------------------------
%%% data conversion helpers

%% @doc Helper to convert string to integer.
%%
%%   Doesn't die on invalid argument.

-spec make_integer(string()) ->
  {ok, integer()} | {error, badarg}.

make_integer(String) ->
  try list_to_integer(String) of
    Integer -> {ok, Integer}
  catch
    error:badarg -> {error, badarg}
  end.

%%%---------------------------------------------------------------------------
%%% printing helpers

%% @private
%% @doc Print a string to STDOUT, ending it with a new line.

-spec println(iolist() | binary()) ->
  ok.

println(Line) ->
  io:put_chars([Line, $\n]).

%% @private
%% @doc Print an error to STDERR, ending it with a new line.

-spec printerr(iolist() | binary()) ->
  ok.

printerr(Line) ->
  printerr(Line, []).

%% @private
%% @doc Print an error (with some context) to STDERR, ending it with a new
%%   line.

-spec printerr(iolist() | binary(), [{Name, Value}]) ->
  ok
  when Name :: atom(),
       Value :: atom() | string() | binary() | integer().

printerr(Line, InfoFields) ->
  Info = [
    [" ", format_info_field(Name, Value)] ||
    {Name, Value} <- InfoFields
  ],
  case Info of
    [] -> io:put_chars(standard_error, [Line, $\n]);
    _  -> io:put_chars(standard_error, [Line, $:, Info, $\n])
  end.

%% @doc Helper for {@link printerr/2}.

-spec format_info_field(atom(), Value) ->
  iolist()
  when Value :: atom() | string() | binary() | integer().

format_info_field(Name, Value) when is_list(Value); is_binary(Value) ->
  case re:run(Value, "^[a-zA-Z0-9~@_+:,./-]*$", [{capture, none}]) of
    match ->
      [atom_to_list(Name), $=, Value];
    nomatch ->
      PrintValue = re:replace(Value, "[\"\\\\]", "\\\\&", [global]),
      [atom_to_list(Name), $=, $", PrintValue, $"]
  end;
format_info_field(Name, Value) when is_integer(Value) ->
  [atom_to_list(Name), $=, integer_to_list(Value)];
format_info_field(Name, Value) when is_atom(Value) ->
  [atom_to_list(Name), $=, atom_to_list(Value)].

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
