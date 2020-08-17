#include <syslog.h>
#include <string.h>
#include "erl_nif.h"

#define ATOMS                                   \
    ATOM_DECL(user, LOG_USER);                  \
    ATOM_DECL(undefined, 0);                    \
    ATOM_DECL(auth, LOG_AUTH);                  \
    ATOM_DECL(authpriv, LOG_AUTHPRIV);          \
    ATOM_DECL(cron, LOG_CRON);                  \
    ATOM_DECL(daemon, LOG_DAEMON);              \
    ATOM_DECL(ftp, LOG_FTP);                    \
    ATOM_DECL(kern, LOG_KERN);                  \
    ATOM_DECL(local0, LOG_LOCAL0);              \
    ATOM_DECL(local1, LOG_LOCAL1);              \
    ATOM_DECL(local2, LOG_LOCAL2);              \
    ATOM_DECL(local3, LOG_LOCAL3);              \
    ATOM_DECL(local4, LOG_LOCAL4);              \
    ATOM_DECL(local5, LOG_LOCAL5);              \
    ATOM_DECL(local6, LOG_LOCAL6);              \
    ATOM_DECL(local7, LOG_LOCAL7);              \
    ATOM_DECL(lpr, LOG_LPR);                    \
    ATOM_DECL(mail, LOG_MAIL);                  \
    ATOM_DECL(news, LOG_NEWS);                  \
    ATOM_DECL(syslog, LOG_SYSLOG);              \
    ATOM_DECL(uucp, LOG_UUCP);

#define ATOM_DECL(A,B) static ERL_NIF_TERM atom_##A
ATOMS
#undef ATOM_DECL

static ErlNifBinary ident;

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{

#define ATOM_DECL(A,B) atom_##A = enif_make_atom(env, #A)
ATOMS
#undef ATOM_DECL

    *priv_data = NULL;

    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{

}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data,
        ERL_NIF_TERM load_info)
{
    if (*old_priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (*priv_data != NULL) {
        return -1; /* Don't know how to do that */
    }
    if (load(env, priv_data, load_info)) {
        return -1;
    }
    return 0;
}

static int
get_facility(ErlNifEnv* env, const ERL_NIF_TERM facility)
{
#define ATOM_DECL(A,B) if (enif_compare(facility, atom_##A) == 0) return B
ATOMS
#undef ATOM_DECL
    return 0;
}

static ERL_NIF_TERM
open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int facility = get_facility(env, argv[2]);
    int logopts = 0;
    ERL_NIF_TERM value;

    if (enif_inspect_iolist_as_binary(env, argv[0], &ident) == 0)
        return enif_make_badarg(env);

#ifdef LOG_CONS
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "cons"), &value))
        logopts |= LOG_CONS;
#endif

#ifdef LOG_NDELAY
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "ndelay"), &value))
        logopts |= LOG_NDELAY;
#endif

#ifdef LOG_NOWAIT
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "nowait"), &value))
        logopts |= LOG_NOWAIT;
#endif

#ifdef LOG_ODELAY
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "odelay"), &value))
        logopts |= LOG_ODELAY;
#endif

#ifdef LOG_PERROR
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "perror"), &value))
        logopts |= LOG_PERROR;
#endif

#ifdef LOG_PID
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "pid"), &value))
        logopts |= LOG_PID;
#endif

    openlog((char *)ident.data, logopts, facility);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int facility = get_facility(env, argv[0]);
    ERL_NIF_TERM res, level_map;
    ERL_NIF_TERM lm_keys[] = {
        enif_make_atom(env, "emergency"),
        enif_make_atom(env, "alert"),
        enif_make_atom(env, "critical"),
        enif_make_atom(env, "error"),
        enif_make_atom(env, "warning"),
        enif_make_atom(env, "notice"),
        enif_make_atom(env, "info"),
        enif_make_atom(env, "debug")
    };
    ERL_NIF_TERM lm_values[] = {
        enif_make_int(env, LOG_EMERG),
        enif_make_int(env, LOG_ALERT),
        enif_make_int(env, LOG_CRIT),
        enif_make_int(env, LOG_ERR),
        enif_make_int(env, LOG_WARNING),
        enif_make_int(env, LOG_NOTICE),
        enif_make_int(env, LOG_INFO),
        enif_make_int(env, LOG_DEBUG)
    };

    if (enif_make_map_from_arrays(env, lm_keys, lm_values,
                                  sizeof(lm_values) / sizeof(*lm_values),
                                  &level_map)) {
        ERL_NIF_TERM res_keys[] = {
            enif_make_atom(env, "bfacility"),
            enif_make_atom(env, "level_map")
        };
        ERL_NIF_TERM res_values[] = {
            enif_make_int(env, facility),
            level_map
        };
        if (enif_make_map_from_arrays(env, res_keys, res_values,
                                      sizeof(res_values) / sizeof(*res_values),
                                      &res))
            return res;
    }
    return enif_make_badarg(env);
}

static ERL_NIF_TERM
do_syslog(ErlNifEnv* env,  int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary bin;
    int levelfacility;

    if (enif_inspect_iolist_as_binary(env, argv[1], &bin) == 0)
        return enif_make_badarg(env);

    if (enif_get_int(env, argv[0], &levelfacility) == 0)
        return enif_make_badarg(env);

    syslog(levelfacility, "%s", bin.data);

    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"syslog_open", 3, open},
    {"syslog", 2, do_syslog},
    {"syslog_init", 1, init},
};

ERL_NIF_INIT(syslogger, nif_funcs, load, NULL, upgrade, unload);
