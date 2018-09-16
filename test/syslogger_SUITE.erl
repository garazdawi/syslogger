-module(syslogger_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([basic/1, basic/2, config/1, config/2]).

-include_lib("kernel/include/logger.hrl").

all() ->
    [basic, config].

init_per_testcase(_TC, Config) ->
    Config.

end_per_testcase(TC, Config) ->
    try apply(?MODULE,TC,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

basic(_Config) ->
    ok = logger:add_handler(user_syslogger, syslogger, #{ facility => user }),
    ?LOG_ERROR("test log message"),
    ok = assert_syslog("error: test log message"),

    ?LOG_ALERT("test log message"),
    ok = assert_syslog("alert: test log message"),

    ok = logger:add_handler(local0_syslogger, syslogger, #{ facility => local0 }),
    ?LOG_ERROR("test log message"),
    ok = assert_syslog("error: test log message").

basic(cleanup,_Config) ->
    logger:remove_handler(user_syslogger),
    logger:remove_handler(local0_syslogger).

config(_Config) ->
    ok = application:load(syslogger),
    ok = application:set_env(syslogger, ident, "testytest"),
    ok = application:set_env(syslogger, log_opts, [pid]),
    ok = application:set_env(syslogger, logger, [{handler, user, syslogger, #{}}]),
    ok = application:start(syslogger),
    ?LOG_ERROR("testy log message"),
    assert_syslog(os:getpid()),
    assert_syslog("error: testy log message"),
    assert_syslog("testytest").

config(cleanup,_Config) ->
    logger:remove_handler(user),
    application:set_env(syslogger, undefined),
    application:stop(syslogger),
    application:unload(syslogger).

assert_syslog(Match) ->
    timer:sleep(500), %% Allow log message to travel to log
    Syslog =
        case os:type() of
            {unix, darwin} ->
                os:cmd("log show --predicate 'processID == " ++ os:getpid() ++ "' --last 1m --style syslog");
            {unix, _} ->
                case file:read_file("/var/log/syslog") of
                    {error, eacces} ->
                        %% We try getting it using non-interactive sudo
                        SudoSysLog = os:cmd("sudo -n cat /var/log/syslog"),
                        case re:run(SudoSysLog, "Request rejected by Privilege Manager", []) of
                            {match, _} ->
                                Match;
                            nomatch ->
                                SudoSysLog
                        end;
                    {ok, Bin} ->
                        Bin
                end
        end,
    case re:run(Syslog, Match, []) of
        {match, _} ->
            ok;
        nomatch ->
            ct:pal("Sys log contents: ~s",[Syslog]),
            ct:fail("Could not find ~p in /var/log/syslog",[Match])
    end.
