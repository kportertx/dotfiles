case $MYSHELL in
    "bash")
        # don't put duplicate lines or lines starting with space in the history.
        # See bash(1) for more options
        HISTCONTROL=ignoreboth

        # append to the history file, don't overwrite it
        shopt -s histappend

        # for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
        HISTSIZE=10000
        HISTFILESIZE=20000

        # check the window size after each command and, if necessary,
        # update the values of LINES and COLUMNS.
        shopt -s checkwinsize

        # If set, the pattern "**" used in a pathname expansion context will
        # match all files and zero or more directories and subdirectories.
        shopt -s globstar

        # make less more friendly for non-text input files, see lesspipe(1)
        [ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

        # set variable identifying the chroot you work in (used in the prompt below)
        if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
            debian_chroot=$(cat /etc/debian_chroot)
        fi

        # If this is an xterm set the title to user@host:dir
        case "$TERM" in
            xterm*|rxvt*)
                PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
                ;;
            *)
                ;;
        esac
        ;;
    "zsh")
        export HISTFILE=~/.zsh_history
        export SAVEHIST=10000
        export HISTFILESIZE=50000
        export HISTSIZE=20000
        export HISTTIMEFORMAT="[%F %T] "

        setopt extended_history       # record timestamp of command in HISTFILE
        setopt hist_expire_dups_first # delete duplicates first when HISTFILE size exceeds HISTSIZE
        setopt hist_ignore_dups       # ignore duplicated commands history list
        setopt hist_ignore_space      # ignore commands that start with space
        setopt hist_verify            # show command with history expansion to user before running it
        setopt inc_append_history_time # share command history data
        ;;
    *)
        printf "$0 - Unexpected SHELL \"$MYSHELL\""
        ;;
esac
