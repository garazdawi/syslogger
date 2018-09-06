%%%-------------------------------------------------------------------
%% @doc syslogger top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(syslogger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ok = syslogger:open(
           application:get_env(syslogger, ident, undefined),
           application:get_env(syslogger, log_opts, undefined),
           application:get_env(syslogger, facility, undefined)
          ),
    case application:get_env(syslogger, handlers) of
        {ok, Handlers} ->
            lists:foreach(fun add_handler/1, Handlers);
        undefined ->
            ok
    end,
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================

add_handler({HandlerId, Config}) ->
    ok = logger:add_handler(HandlerId, syslogger, Config).
