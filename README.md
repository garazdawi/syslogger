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

The default configuration is then used and any log messages will turn up
in the local syslog.

Configuration
-------------

The syslogger application can be used either by configuring the handlers through
application variables, or by using the `logger:add_handler/2` API.

For instance if you want to add two handlers that log to two different syslog
facilities just add this to your sys.config.

    {syslogger,
      {handler, [{user_syslogger, #{ facility => user }},
                 {local0_syslogger, #{ facility => local0 }}]
       }
    }.

This will add two syslogger instances to the Erlang logger that use different facilities.

The same effect could have been achieved by using the logger API like this:

    logger:add_handler(user_syslogger, syslogger, #{ facility => user }),
    logger:add_handler(local0_syslogger, syslogger, #{ facility => local0 }).

Each instance of the syslogger backend can be configured using a map with these
configuration options:

- `ident`: The syslog identifier that is prepended to each log message.
  - Default: The value of `init:get_argument(progname)`.
- `facility`: The syslog facility to log though.
  - Default: user
- `log_opts`: The syslog options (as a list) to use.
  - Default: [cons, pid, perror]

For more details of what each of these options do see https://github.com/Vagabond/erlang-syslog.
