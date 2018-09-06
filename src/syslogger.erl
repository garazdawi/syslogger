%%%-------------------------------------------------------------------
%% @doc syslogger handler
%% @end
%%%-------------------------------------------------------------------

-module(syslogger).

-on_load(init/0).
-define(APPNAME, syslogger).
-define(LIBNAME, libsyslogger).

-export([log/2, open/3]).

log(Log = #{ level := Level }, #{ formatter := {FModule, FConfig},
                                  facility := Facility }) ->
    syslog(Level, Facility, unicode:characters_to_binary(
            io_lib:format("~ts\0",[FModule:format(Log, FConfig)]))).

syslog(emergency, F, Str) ->
    log_emergency(F, Str);
syslog(alert, F, Str) ->
    log_alert(F, Str);
syslog(critical, F, Str) ->
    log_critical(F, Str);
syslog(error, F, Str) ->
    log_error(F, Str);
syslog(warning, F, Str) ->
    log_warning(F, Str);
syslog(notice, F, Str) ->
    log_notice(F, Str);
syslog(info, F, Str) ->
    log_info(F, Str);
syslog(debug, F, Str) ->
    log_debug(F, Str).

open(undefined, LogOpts, Facility) ->
    {ok, [Progname]} = init:get_argument(progname),
    open(filename:basename(Progname), LogOpts, Facility);
open(Ident, LogOpts, Facility) ->
    syslog_open(Ident, maps:from_list([{LogOpt, true} || LogOpt <- LogOpts]), Facility).

syslog_open(_Ident, _LogOpts, _Facility) ->
    not_loaded(?LINE).

log_open(_Ident, _LogOpts, _Facility) ->
    not_loaded(?LINE).

log_emergency(_F, _Str) -> not_loaded(?LINE).
log_alert(_F, _Str) -> not_loaded(?LINE).
log_critical(_F, _Str) -> not_loaded(?LINE).
log_error(_F, _Str) -> not_loaded(?LINE).
log_warning(_F, _Str) -> not_loaded(?LINE).
log_notice(_F, _Str) -> not_loaded(?LINE).
log_info(_F, _Str) -> not_loaded(?LINE).
log_debug(_F, _Str) -> not_loaded(?LINE).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?LIBNAME]);
                _ ->
                    filename:join([priv, ?LIBNAME])
            end;
        Dir ->
            filename:join([Dir, ?LIBNAME])
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
