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


static ERL_NIF_TERM
open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
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
log_priority(ErlNifEnv* env, int level, const ERL_NIF_TERM facility, const ERL_NIF_TERM msg)
{
    int facility = get_facility(env, argv[2]);
    int logopts = 0;
    ErlNifBinary bin;
    ERL_NIF_TERM value;
    char *ident;

    if (enif_inspect_iolist_as_binary(env, argv[0], &bin) == 0)
        return enif_make_badarg(env);

    /* openlog uses the given buffer, so we have to make a copy of it. */
    ident = enif_alloc(bin.size + 1);
    memcpy(ident, bin.data, bin.size);
    ident[bin.size] = '\0';

#ifdef LOG_CONS
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "cons"), &value))
        logopts |= LOG_CONS;
#endif

#ifdef LOG_NDELAY
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "ndelay"), &value))
        logopts |= LOG_NDELAY;
#endif

#ifdef LOG_PERROR
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "perror"), &value))
        logopts |= LOG_PERROR;
#endif

#ifdef LOG_PID
    if (enif_get_map_value(env, argv[1], enif_make_atom(env, "pid"), &value))
        logopts |= LOG_PID;
#endif

    openlog(ident, logopts, facility);
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM
log_emergency(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return log_priority(env, LOG_EMERG, argv[0], argv[1]);
}

static ERL_NIF_TERM
log_alert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return log_priority(env, LOG_ALERT, argv[0], argv[1]);
}

static ERL_NIF_TERM
log_critical(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return log_priority(env, LOG_CRIT, argv[0], argv[1]);
}

static ERL_NIF_TERM
log_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return log_priority(env, LOG_ERR, argv[0], argv[1]);
}

static ERL_NIF_TERM
log_warning(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return log_priority(env, LOG_WARNING, argv[0], argv[1]);
}

static ERL_NIF_TERM
log_notice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return log_priority(env, LOG_NOTICE, argv[0], argv[1]);
}

static ERL_NIF_TERM
log_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return log_priority(env, LOG_INFO, argv[0], argv[1]);
}

static ERL_NIF_TERM
log_debug(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return log_priority(env, LOG_DEBUG, argv[0], argv[1]);
}

static ErlNifFunc nif_funcs[] = {
    {"log_open", 3, open},
    {"log_emergency", 2, log_emergency},
    {"log_alert", 2, log_alert},
    {"log_critical", 2, log_critical},
    {"log_error", 2, log_error},
    {"log_warning", 2, log_warning},
    {"log_notice", 2, log_notice},
    {"log_info", 2, log_info},
    {"log_debug", 2, log_debug}
};

ERL_NIF_INIT(syslogger, nif_funcs, load, NULL, upgrade, unload);
