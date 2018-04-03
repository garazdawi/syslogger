%%%-------------------------------------------------------------------
%% @doc syslogger handler
%% @end
%%%-------------------------------------------------------------------

-module(syslogger).

-export([adding_handler/2, log/2]).

adding_handler(Id, #{ ident := Ident, log_opts := LogOpts,
                      facility := Facility} = Config) ->
    spawn(fun() -> ok = logger:set_formatter(Id, logger_formatter, #{single_line => true}) end),
    {ok, LogHandle} = syslog:open(Ident, LogOpts, Facility),
    {ok, Config#{ handle => LogHandle }};
adding_handler(Id, Config) ->
    adding_handler(Id, maps:merge(Config, default_config())).

log(#{msg := {report, R}, meta := Meta } = Log, Config) ->
    Fun = maps:get(report_cb, Meta, fun default_report_cb/1),
    log(Log#{ msg := Fun(R) }, Config);
log(Log, #{ handle := Handle, formatter := {FModule, FConfig}}) ->
    syslog:log(Handle, level(Log), "~s", [FModule:format(Log, FConfig)]).

level(#{ level := Level }) ->
    level(Level);
level(emergency) ->
    emerg;
level(critical) ->
    crit;
level(error) ->
    err;
level(A) ->
    A.

default_config() ->
    {ok, [Progname]} = init:get_argument(progname),
    #{ ident => filename:basename(Progname),
       log_opts => [cons, pid, perror],
       facility => user
     }.

default_report_cb(R) ->
    {"~p",[R]}.
