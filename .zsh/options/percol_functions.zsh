function percol_select_history() {
    local tac_cmd
    which gtac >& /dev/null && tac_cmd=gtac || tac_cmd=tac
    BUFFER=$($tac_cmd ~/.zsh_history | sed 's/^: [0-9]*:[0-9]*;//' \
        | percol --match-method regex --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle -R -c
}
zle -N percol_select_history
bindkey '^R' percol_select_history
