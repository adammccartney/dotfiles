# logging

PAS="[ PASS ] "
ERR="[ FAIL ] "
WAR="[ WARN ] "
INF="[ INFO ] "

#+UTILITIES

_err()
{ # All errors go to stderr.
    printf "%s: %s\n" "$(date -u +"%Y-%m-%dT%H:%M:%S.%3NZ")" "$1"
}

_msg()
{ # Default message to stdout.
    printf "%s: %s\n" "$(date -u +"%Y-%m-%dT%H:%M:%S.%3NZ")" "$1"
}

_debug()
{
    if [ "${DEBUG}" = '1' ]; then
        printf "%s: %s\n" "$(date -u +"%Y-%m-%dT%H:%M:%S.%3NZ")" "$1"
    fi
}

die()
{
    _err "${ERR}$*"
    exit 1
}


