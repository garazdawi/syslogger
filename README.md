syslogger
=====

A syslog backend for erlang logger.

Usage
-----

The easiest way to use syslogger is to just include it as a dependency
in your rebar.config like this:

    {deps,
     [
      { 'syslogger', "*", {git, "git://github.com/garazdawi/syslogger", {branch, "master"}}}
     ]
    }.

The default configuration is then used and any log messages will turn up in the local syslog.

Build Requirements
-------------------

You need to have Erlang/OTP, a C compiler, rebar3 and the autoconf toolchain installed.

    apt-get install build-essential autoconf libtool

Recommended Configuration
-------------------------

If you just want to get started with syslogger, the recommended configuration in
the system's sys.config is:

    {kernel, logger, [{handler, default, undefined}]}.
    {syslogger,
      {log_opts, [cons, pid, perror]},
      {logger, [{handler, default, syslogger,
                   #{ formatter => {logger_formatter, #{single_line => true}}}}]}
    }.

Configuration
-------------

The syslogger application can be used either by configuring the handlers through
application variables, or by using the `logger:add_handler/2` API.

For instance if you want to add two handlers that log to two different syslog
facilities just add this to your sys.config.

    {syslogger,
      {ident, "myapp"},
      {log_opts, [cons, pid, perror]},
      {logger, [{handler, user_syslogger, syslogger, #{ facility => user }},
                {handler, local0_syslogger, syslogger, #{ facility => local0 }}]
       }
    }.

This will add two syslogger instances to the Erlang logger that use different facilities.

The same effect could have been achieved by using the logger API like this:

    logger:add_handler(user_syslogger, syslogger, #{ facility => user }),
    logger:add_handler(local0_syslogger, syslogger, #{ facility => local0 }).

Each syslogger handler can be configured using a map with these configuration options:

- `facility`: The syslog facility to log though.
  - Default: undefined i.e. the default set by openlog or the system default.

The openlog call can get the following init at startup:

- `ident`: The syslog identifier that is prepended to each log message.
  - Default: The value of `init:get_argument(progname)`.
- `facility`: The syslog facility to log though.
  - Default: user
- `log_opts`: The syslog options (as a list) to use.
  - Default: []

For more details of what each of these options do see [syslog(3)](https://linux.die.net/man/3/syslog).
