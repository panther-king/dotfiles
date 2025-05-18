# 変数の重複を除去
typeset -U path PATH fpath FPATH

# コマンドエイリアス
alias ls="ls --color=auto --group-directories-first"
alias ll="ls -lh"
alias la="ll -A"
alias vi="vim"

# 画像とPDFファイルを直接開けるように
alias -s {bmp,gif,jpg,jpeg,png,tiff,BMP,GIF,JPG,JPEG,PNG,TIFF}=viewnior
alias -s pdf=mupdf

# rust用環境変数
source $HOME/.cargo/env
export LD_LIBRARY_PATH=$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH

# dotnetコマンドパス
[ -d ~/.dotnet ] && export PATH=$PATH:$HOME/.dotnet

# ghcup
[ -f "/home/i/.ghcup/env" ] && . "/home/i/.ghcup/env"

# docker-rootless
export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/docker.sock

# direnv
eval "$(direnv hook zsh)"

# starship
eval "$(starship init zsh)"

# lessをカラー表示
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS='-R '

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust

### End of Zinit's installer chunk

# コマンド履歴
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=1000000

setopt inc_append_history       # 履歴を追加
setopt share_history            # 履歴をリアルタイム共有
setopt hist_ignore_all_dups     # historyで重複は非表示
setopt hist_save_no_dups        # 同じコマンドの保存は古い方を削除
setopt extended_history         # 実行時のタイムスタンプを記録
setopt hist_expire_dups_first   # HISTFILEのサイズがHISTSIZEを超えたら、まず重複を除去

# 補完
zinit ice wait'!0'; zinit light zsh-users/zsh-completions
autoload -Uz compinit && compinit

zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-autosuggestions

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'    # 補完はignore case
zstyle ':completion:*:default' menu select=2           # 候補をTabで選択可能に
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}  # ファイル補完候補に色付け

ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=242"  # 補完色

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-max 1000
zstyle ':chpwd:*' recent-dirs-file $HOME/.cache/chpwd-recent-dirs
zstyle ':chpwd:*' recent-dirs-prune 'parent'
zstyle ':completion:*' recent-dirs-insert always

setopt auto_param_slash      # ディレクトリ名補完で末尾のスラッシュも付与
setopt auto_param_keys       # カッコを自動補完
setopt mark_dirs             # ファイル名展開でディレクトリマッチ時は末尾スラッシュを補完
setopt auto_menu             # 補完キーで自動補完
setopt magic_equal_subst     # ロングオプションの引数も補完可能に
setopt auto_cd               # ディレクトリ名のみでcd
setopt correct               # スペルミス補正
setopt interactive_comments  # CLIでも'#'以降をコメントとみなす

# @refs https://github.com/catppuccin/skim
export SKIM_DEFAULT_OPTIONS="$SKIM_DEFAULT_OPTIONS \
--color=fg:#cdd6f4,matched:#313244,matched_bg:#f2cdcd,current:#cdd6f4,current_bg:#45475a,current_match:#1e1e2e,current_match_bg:#f5e0dc,spinner:#a6e3a1,info:#cba6f7,prompt:#89b4fa,cursor:#f38ba8,selected:#eba0ac,header:#94e2d5,border:#6c7086"

# sk(skim)でコマンド履歴を検索
function sk-history () {
    BUFFER=`history -n 1 | sk`
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N sk-history
bindkey '^R' sk-history

# ghq + sk でリポジトリ移動
function sk-ghq () {
    local selected_dir=$(ghq list -p | sk --prompt="ghq > " --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N sk-ghq
bindkey '^]' sk-ghq

# cdr + sk でディレクトリ移動
function sk-cdr () {
    local selected_dir=$(cdr -l | sed 's/^[0-9]\+ \+//' | sk --prompt="cdr > " --query "$LBUFFER")
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N sk-cdr
bindkey '^[' sk-cdr

# zellij
eval "$(zellij setup --generate-auto-start zsh)"

# mise
eval "$(/usr/bin/mise activate zsh)"
