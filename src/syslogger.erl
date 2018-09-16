%%%-------------------------------------------------------------------
%% @doc syslogger handler
%% @end
%%%-------------------------------------------------------------------

-module(syslogger).

-on_load(init/0).
-define(APPNAME, syslogger).
-define(LIBNAME, libsyslogger).

-export([adding_handler/1, log/2, open/3]).

adding_handler(#{ facility := Facility } = Config) ->
    SysLoggerConfig = syslog_init(Facility),
    {ok, Config#{ syslogger => SysLoggerConfig} };
adding_handler(Config) ->
    adding_handler(Config#{ facility => undefined }).


log(Log = #{ level := Level }, #{ formatter := {FModule, FConfig},
                                  syslogger :=
                                      #{bfacility := Facility,
                                        level_map := LevelMap
                                       }
                                }) ->
    syslog(Facility bor maps:get(Level, LevelMap),
           unicode:characters_to_binary(
             io_lib:format("~ts\0",[FModule:format(Log, FConfig)]))).

open(undefined, LogOpts, Facility) ->
    {ok, [Progname]} = init:get_argument(progname),
    open(filename:basename(Progname), LogOpts, Facility);
open(Ident, LogOpts, Facility) ->
    syslog_open([Ident,$\0], LogOpts, Facility).

syslog_open(_Ident, _LogOpts, _Facility) ->
    not_loaded(?LINE).

syslog_init(_Facility) ->
    not_loaded(?LINE).

syslog(_LevelFacility, _Str) -> not_loaded(?LINE).

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
