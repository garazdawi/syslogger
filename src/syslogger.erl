-module(syslogger).

-on_load(load/0).

-export([adding_handler/1, log/2, open/3]).

default_config() ->
    #{facility => undefined}.

adding_handler(Config) ->
    HConfig0 = maps:get(config, Config, #{}),
    HConfig = maps:merge(default_config(), HConfig0),
    SysLoggerConfig = syslog_init(maps:get(facility, HConfig)),
    {ok, Config#{ config => maps:merge(SysLoggerConfig, HConfig)} }.


log(Log = #{ level := Level }, #{ formatter := {FModule, FConfig},
                                  config :=
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
open(Ident, undefined, Facility) ->
    open(Ident, [], Facility);
open(Ident, LogOpts, Facility) ->
    MapLogOpts = maps:from_list(proplists:unfold(LogOpts)),
    Id = iolist_to_binary(Ident ++ [$\0]),
    syslog_open(Id, MapLogOpts, Facility).

syslog_open(_Ident, _LogOpts, _Facility) ->
    not_loaded(?LINE).

syslog_init(_Facility) ->
    not_loaded(?LINE).

syslog(_LevelFacility, _Str) ->
    not_loaded(?LINE).

load() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, libsyslogger]);
                _ ->
                    filename:join([priv, libsyslogger])
            end;
        Dir ->
            filename:join([Dir, libsyslogger])
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).
