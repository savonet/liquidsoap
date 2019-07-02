if [ -z "$BASH_VERSION" ]; then return 0; fi

_liquidsoap_add()
{
    IFS=$'\n' _liquidsoap_reply+=("$@")
}

_liquidsoap_add_f()
{
    local cmd
    cmd=$1; shift
    _liquidsoap_add "$($cmd "$@" 2>/dev/null)"
}

_liquidsoap()
{
    local IFS cmd cur compgen_opt

    cmd=${COMP_WORDS[1]}
    cur=${COMP_WORDS[COMP_CWORD]}
    prev=${COMP_WORDS[COMP_CWORD-1]}
    compgen_opt=()
    _liquidsoap_reply=()

    case "$prev" in
        "-h")
            _liquidsoap_add_f ../src/liquidsoap --list-functions
            ;;

        *)
            _liquidsoap_reply+=("--conf-descr -h --help -i --interactive --list-functions-md --list-plugins --quiet -r --version")
            ;;
    esac

    COMPREPLY=($(compgen -W "${_liquidsoap_reply[*]}" "${compgen_opt[@]}" -- "$cur"))
    unset _liquidsoap_reply
    return 0
}

complete -F _liquidsoap liquidsoap
