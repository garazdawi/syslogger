%%%-------------------------------------------------------------------
%% @doc syslogger handler
%% @end
%%%-------------------------------------------------------------------

-module(syslogger).

-export([adding_handler/2, log/2]).

adding_handler(_Id, #{ ident := Ident, log_opts := LogOpts,
                       facility := Facility} = Config) ->
    {ok, LogHandle} = syslog:open(Ident, LogOpts, Facility),
    {ok, Config#{ handle => LogHandle,
                  formatter := {logger_formatter, #{single_line => true}}}};
adding_handler(Id, Config) ->
    adding_handler(Id, maps:merge(Config, default_config())).

log(Log, #{ handle := Handle, formatter := {FModule, FConfig}}) ->
    syslog:log(Handle, level(Log),
               unicode:characters_to_binary(
                 io_lib:format("~ts",[FModule:format(Log, FConfig)]))).

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
