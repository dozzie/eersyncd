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

%% interface for eersyncd_command_handler
-export([reload/1]).

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
      | reopen_logs
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
  case config_load(ConfigPath) of
    {ok, AppEnv, IndiraOpts, ErrorLoggerLog} ->
      case ErrorLoggerLog of
        undefined -> ok;
        _ -> ok = indira_disk_h:install(error_logger, ErrorLoggerLog)
      end,
      eeindira:set_reload({?MODULE, reload, [ConfigPath]}),
      eeindira:set_env(eersyncd, AppEnv),
      indira_app:daemonize(eersyncd, [
        {listen, [{?ADMIN_SOCKET_TYPE, Socket}]},
        {command, {?ADMIN_COMMAND_MODULE, []}},
        {start_before, SASLApp},
        {pidfile, PidFile} |
        IndiraOpts
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
    {error, Reason} ->
      {error, Reason};
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

%% emulate target gen_indira_cli error reporting (verbatim, not tuple-wrapped)
format_error({arguments, Reason}) ->
  format_error(Reason);
format_error({format, Reason}) ->
  format_error(Reason);

%% errors from `parse_arguments()' (own and `cli_opt()' + argument)
format_error(too_many_args) ->
  "too many arguments for the command";
format_error({{bad_timeout, Value}, _Arg}) ->
  ["bad timeout value: ", Value];
format_error({bad_option, Option}) ->
  ["unrecognized option: ", Option];
format_error({bad_command, Command}) ->
  ["unrecognized command: ", Command];
format_error({excessive_value, Option}) -> % from `indira_cli:foldg()'
  ["unexpected argument for ", Option];
format_error({not_enough_args, Option}) -> % from `indira_cli:folds()'
  ["missing argument for ", Option];

%% errors from `config_load()'
format_error({config, validate, {Section, Key, Line} = _Where}) ->
  ["config error: invalid value of ", format_toml_key(Section, Key),
    " (line ", integer_to_list(Line), ")"];
format_error({config, toml, Reason}) ->
  ["config error: ", toml:format_error(Reason)];
format_error({config, {missing, Section, Key}}) ->
  ["config error: missing required key ", format_toml_key(Section, Key)];

%% errors from `indira_cli:execute()' (TODO: `indira_cli:format_error()')
format_error({send, bad_request_format}) ->
  "can't serialize command request (programmer's error)";
format_error({send, bad_reply_format}) -> % programmer error
  "can't deserialize reply to the command (programmer's error)";
format_error({send, timeout}) ->
  "command request: operation timed out";
format_error({send, badarg}) ->
  "bad admin socket address (programmer's error)";
format_error({send, Posix}) ->
  ["command request: ", inet:format_error(Posix)];
format_error({bad_return_value, Return}) ->
  ["callback returned unexpected value ", format_term(Return),
    " (programmer's error)"];

%% errors from `indira_app:daemonize()' (TODO: `indira_app:format_error()',
%% wrap these in a tagged tuple for recognition (`daemonize()',
%% `distributed_reconfigure()', `indira_setup()'))
format_error(invalid_listen_spec) ->
  "Indira setup: invalid listen address specification (programmer's error)";
format_error(missing_listen_spec) ->
  "Indira setup: no listen address specified (programmer's error)";
format_error(invalid_command_handler) ->
  "Indira setup: invalid admin command handler name (programmer's error)";
format_error(missing_command_handler) ->
  "Indira setup: no admin command handler specified (programmer's error)";
format_error(invalid_pidfile) ->
  "Indira setup: pidfile name (programmer's error)";
format_error(invalid_net_config) ->
  "Indira setup: invalid Erlang networking configuration (programmer's error)";
format_error(invalid_net_start) ->
  "Indira setup: invalid Erlang networking autostart flag (programmer's error)";

%% `handle_reply()'
format_error(Reason) when is_binary(Reason) ->
  % error message ready to print to the console
  Reason;
format_error({unknown_status, Status}) ->
  % `$SCRIPT status' returned unrecognized status
  ["unrecognized status: ", format_term(Status)];
format_error(unrecognized_reply) -> % see `?ADMIN_COMMAND_MODULE:parse_reply()'
  "unrecognized reply to the command (programmer's error)";
format_error('TODO') ->
  "command not implemented yet";

format_error(Reason) ->
  ["unrecognized error: ", format_term(Reason)].

%% @doc Serialize an arbitrary term to a single line of text.

-spec format_term(term()) ->
  iolist().

format_term(Term) ->
  io_lib:print(Term, 1, 16#ffffffff, -1).

%% @doc Format TOML path (section + key name) for error message.

-spec format_toml_key([string()], string()) ->
  iolist().

format_toml_key(Section, Key) ->
  [$", [[S, $.] || S <- Section], Key, $"].

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
%%% loading configuration
%%%---------------------------------------------------------------------------

%% @doc Update application environment from config file and instruct
%%   application's processes to reread their parameters.

-spec reload(file:filename()) ->
  ok | {error, term()}.

reload(Path) ->
  case config_load(Path) of
    {ok, AppEnv, IndiraOpts, ErrorLoggerLog} ->
      case ErrorLoggerLog of
        undefined -> ok;
        _ -> ok = indira_disk_h:reopen(error_logger, ErrorLoggerLog)
      end,
      case indira_app:distributed_reconfigure(IndiraOpts) of
        ok ->
          eeindira:set_env(eersyncd, AppEnv),
          % TODO: handle (propagate) reload errors
          LogHandlers = proplists:get_value(log_handlers, AppEnv, []),
          % TODO: send reload signal to the candidate modules
          {ok, _ReloadCandidates} = eersyncd_log:reload(LogHandlers),
          ok = eersyncd_tcp_sup:reload(),
          ok;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason} % `{error, {config, ...}}'
  end.

%% @doc Load configuration file.

-spec config_load(file:filename()) ->
  {ok, AppEnv, IndiraOpts, ErrorLoggerLog} | {error, Reason}
  when AppEnv :: [{atom(), term()}],
       IndiraOpts :: [indira_app:daemon_option()],
       ErrorLoggerLog :: file:filename() | undefined,
       Reason :: {config, validate, Where :: term()}
               | {config, toml, toml:toml_error()}
               | {config, {missing, Section :: [string()], Key :: string()}}.

config_load(Path) ->
  case toml:read_file(Path, {fun config_validate/4, []}) of
    {ok, Config} ->
      case config_build(Config, filename:dirname(filename:absname(Path))) of
        {ok, AppEnv, IndiraOpts, ErrorLoggerLog} ->
          {ok, AppEnv, IndiraOpts, ErrorLoggerLog};
        {error, {missing, _, _} = Reason} ->
          {error, {config, Reason}}
      end;
    {error, {validate, Where, badarg}} ->
      {error, {config, validate, Where}};
    {error, Reason} ->
      {error, {config, toml, Reason}}
  end.

%%----------------------------------------------------------
%% config_build() {{{

%% @doc Build application's and Indira's configuration from values read from
%%   config file.

-spec config_build(toml:config(), file:filename()) ->
  {ok, AppEnv, IndiraOpts, ErrorLoggerLog} | {error, Reason}
  when AppEnv :: [{atom(), term()}],
       IndiraOpts :: [indira_app:daemon_option()],
       ErrorLoggerLog :: file:filename() | undefined,
       Reason :: {missing, Section :: [string()], Key :: string()}.

config_build(TOMLConfig, TOMLDir) ->
  case toml:get_value([], "modules_config", TOMLConfig) of
    none ->
      {error, {missing, [], "modules_config"}};
    {string, RsyncConf} ->
      % if "root_dir" is set, it's relative to `TOMLDir' (unless it's
      % absolute)
      % if "root_dir" is unset, no changes to paths are made (`"."' is a nice
      % no-op path)
      case toml:get_value([], "root_dir", TOMLConfig) of
        {string, RootDirTail} -> RootDir = filename:join(TOMLDir, RootDirTail);
        none -> RootDir = "."
      end,
      AppEnv = lists:foldr(
        fun({Section, Name, EnvKey}, Acc) ->
          case toml:get_value(Section, Name, TOMLConfig) of
            % these two values are paths that need to be made relative to
            % `RootDir', all the other values are either paths naturally
            % relative or not paths at all
            {_Type, Value} when EnvKey == rsync_path;
                                EnvKey == error_logger_file ->
              [{EnvKey, filename:join(RootDir, Value)} | Acc];
            % this path is relative to TOMLDir (unless it's absolute)
            {_Type, Value} when EnvKey == cwd ->
              [{EnvKey, filename:join(TOMLDir, Value)} | Acc];
            {_Type, Value} ->
              [{EnvKey, Value} | Acc];
            none ->
              Acc
          end
        end,
        [{rsyncd_conf, RsyncConf}],
        [ % section, name, `eersyncd' app environment key
          {[],          "root_dir",     cwd},
          {[],          "rsync_path",   rsync_path},
          {["tcp"],     "listen",       listen},
          %{["ssl"],     "listen",       ...}, % TODO
          %{["ssl"],     "cert_file",    ...}, % TODO
          %{["ssl"],     "key_file",     ...}, % TODO
          {["logging"], "rsyncd_log",   rsyncd_log},
          {["logging"], "log_handlers", log_handlers},
          {["logging"], "erlang_log",   error_logger_file}
        ]
      ),
      % TODO: make cookie_file relative to `RootDir'
      IndiraOpts = lists:foldr(
        fun({Name, EnvKey}, Acc) ->
          case toml:get_value(["erlang"], Name, TOMLConfig) of
            {data, Value} -> [{EnvKey, Value} | Acc];
            none -> Acc
          end
        end,
        [],
        [
          {"node_name", node_name},
          {"name_type", name_type},
          {"cookie_file", cookie},
          {"distributed_immediate", net_start}
        ]
      ),
      LogFile = case toml:get_value(["logging"], "erlang_log", TOMLConfig) of
        {string, ErrorLoggerLog} -> filename:join(RootDir, ErrorLoggerLog);
        none -> undefined
      end,
      {ok, AppEnv, IndiraOpts, LogFile}
  end.

%% }}}
%%----------------------------------------------------------
%% config_validate() {{{

%% @doc Validate values read from TOML file (callback for
%%   {@link toml:read_file/2}).

-spec config_validate(Section :: toml:section(), Key :: toml:key(),
                      Value :: toml:toml_value(), Arg :: term()) ->
  ok | {ok, term()} | ignore | {error, badarg}.

config_validate([], "root_dir", Value, _Arg) ->
  case Value of
    {string, [_|_] = _Path} -> ok;
    _ -> {error, badarg}
  end;
config_validate([], "modules_config", Value, _Arg) ->
  case Value of
    {string, [_|_] = _Path} -> ok;
    _ -> {error, badarg}
  end;
config_validate([], "rsync_path", Value, _Arg) ->
  case Value of
    {string, [_|_] = _Path} -> ok;
    _ -> {error, badarg}
  end;

config_validate(["tcp"], "listen", Value, _Arg) ->
  case Value of
    {array, {string, AddrSpecs}} ->
      try lists:map(fun parse_listen/1, AddrSpecs) of
        Addrs -> {ok, Addrs}
      catch
        _:_ -> {error, badarg}
      end;
    {array, {empty, []}} ->
      {ok, []};
    _ ->
      {error, badarg}
  end;

config_validate(["ssl"], "listen", Value, _Arg) ->
  case Value of
    {array, {string, AddrSpecs}} ->
      try lists:map(fun parse_listen/1, AddrSpecs) of
        Addrs -> {ok, Addrs}
      catch
        _:_ -> {error, badarg}
      end;
    {array, {empty, []}} ->
      {ok, []};
    _ ->
      {error, badarg}
  end;
config_validate(["ssl"], "cert_file", Value, _Arg) ->
  case Value of
    {string, [_|_] = _Path} -> ok;
    _ -> {error, badarg}
  end;
config_validate(["ssl"], "key_file", Value, _Arg) ->
  case Value of
    {string, [_|_] = _Path} -> ok;
    _ -> {error, badarg}
  end;

config_validate(["logging"], "log_handlers", Value, _Arg) ->
  case Value of
    {array, {string, Names}} ->
      {ok, [{list_to_atom(N), []} || N <- Names]};
    {array, {empty, []}} ->
      {ok, []};
    _ ->
      {error, badarg}
  end;
config_validate(["logging"], "rsyncd_log", Value, _Args) ->
  case Value of
    {string, [_|_] = _Path} -> ok;
    _ -> {error, badarg}
  end;
config_validate(["logging"], "erlang_log", Value, _Args) ->
  case Value of
    {string, [_|_] = _Path} -> ok;
    _ -> {error, badarg}
  end;

config_validate(["erlang"], "node_name", Value, _Args) ->
  case Value of
    {string, [_|_] = Name} -> {ok, list_to_atom(Name)};
    _ -> {error, badarg}
  end;
config_validate(["erlang"], "name_type", Value, _Args) ->
  case Value of
    {string, "longnames"} -> {ok, longnames};
    {string, "shortnames"} -> {ok, shortnames};
    _ -> {error, badarg}
  end;
config_validate(["erlang"], "cookie_file", Value, _Args) ->
  case Value of
    {string, [_|_] = Path} -> {ok, {file, Path}};
    _ -> {error, badarg}
  end;
config_validate(["erlang"], "distributed_immediate", Value, _Args) ->
  case Value of
    {boolean, V} -> {ok, V};
    _ -> {error, badarg}
  end;

config_validate(_Section, _Key, _Value, _Arg) ->
  ignore.

%% @doc Parse listen address.
%%   Function crashes on invalid address format.

-spec parse_listen(string()) ->
  {any | inet:hostname(), inet:port_number()} | no_return().

parse_listen(AddrPort) ->
  case string:tokens(AddrPort, ":") of
    ["*", Port] -> {any, list_to_integer(Port)};
    [Addr, Port] -> {Addr, list_to_integer(Port)}
  end.

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
      {error, {bad_timeout, Timeout}}
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
  % TODO: add "reopen-logs" command
  case Arg of
    "start"  -> Opts#opts{op = start};
    "status" -> Opts#opts{op = status};
    "stop"   -> Opts#opts{op = stop};
    "reload" -> Opts#opts{op = reload_config};
    "reopen-logs" -> Opts#opts{op = reopen_logs};
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
