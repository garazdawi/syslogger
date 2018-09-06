-module(syslogger_SUITE).

-export([all/0, basic/1]).

-include_lib("kernel/include/logger.hrl").

all() ->
    [basic].

basic(_Config) ->
    ok = logger:add_handler(user_syslogger, syslogger, #{ facility => user }),
    ?LOG_ERROR("test log message"),
    ok = assert_syslog("error: test log message").

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
