# shellcheck disable=SC2034
# 変数の重複を除去
typeset -U path PATH fpath FPATH

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

# Zinit のインストールチェック
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
  command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
  command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git"
fi
source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"

# Zinit の補完
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Zinit の主要拡張
zinit light-mode for \
  zdharma-continuum/zinit-annex-as-monitor \
  zdharma-continuum/zinit-annex-bin-gem-node \
  zdharma-continuum/zinit-annex-patch-dl \
  zdharma-continuum/zinit-annex-rust

# bat
zinit ice as"program" from"gh-r" \
  mv"bat* -> bat" \
  pick"bat/bat" \
  cp"bat/autocomplete/bat.zsh -> _bat"
zinit light sharkdp/bat

# delta
zinit ice as"program" from"gh-r" \
  mv "delta* -> delta_dir" \
  pick"delta_dir/delta"
zinit light dandavison/delta

# fzf (fzf-tabで利用)
zinit ice as"command" from"gh-r" mv"fzf* -> fzf" pick"fzf"
zinit light junegunn/fzf

# pure
zinit ice pick"async.zsh" src"pure.zsh"
zinit light sindresorhus/pure

# ripgrep
zinit ice as"program" from"gh-r" \
  mv"ripgrep* -> rg" \
  pick"rg/rg" \
  cp"rg/complete/_rg -> _rg"
zinit light BurntSushi/ripgrep

# yazi
zinit ice as"program" from"gh-r" \
  mv"yazi* -> yazi_dir" \
  pick"yazi_dir/yazi" \
  cp"yazi_dir/completions/_yazi -> _yazi"
zinit light sxyazi/yazi

# zellij
zinit ice as"program" from"gh-r" \
  atclone"./zellij setup --generate-completion zsh > _zellij" \
  atpull"%atclone"
zinit light zellij-org/zellij

# 補完強化
zinit light zsh-users/zsh-completions

# シンタックスハイライト
zinit ice wait"0" lucid atinit"zpcompinit; zpcdreplay"
zinit light zdharma-continuum/fast-syntax-highlighting

# オートサジェスト
zinit ice wait"0" lucid atload"_zsh_autosuggest_start"
zinit light zsh-users/zsh-autosuggestions

# 補完メニューを fzf で選択
zinit ice wait"0" lucid
zinit light Aloxaf/fzf-tab

# fzf-tab に Catppuccin Mocha テーマを適用
# @refs https://github.com/catppuccin/fzf
zstyle ':fzf-tab:*' fzf-flags \
    --color=bg+:#313244,bg:-1,spinner:#f5e0dc,hl:#f38ba8 \
    --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
    --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8

# 補完システムの初期化
autoload -Uz compinit
if [[ -n ${ZDOTDIR:-$HOME}/.zcompdump(#qN.mh-24) ]]; then
  # 当日作成なのでチェックスキップ
  compinit -C
else
  compinit
fi

zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'    # 補完はignore case
zstyle ':completion:*:default' menu select=2           # 候補をTabで選択可能に
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}  # ファイル補完候補に色付け
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=242"  # 補完色

# mise
if [[ -x /usr/bin/mise ]]; then
  eval "$(/usr/bin/mise activate zsh)"
fi

# Rust用環境変数
if command -v rustc >/dev/null 2>&1; then
  source $HOME/.cargo/env
  typeset -T LD_LIBRARY_PATH ld_library_path
  ld_library_path+=("$(rustc --print sysroot)/lib")
  typeset -U ld_library_path
  ld_library_path=(${ld_library_path:#})
  export LD_LIBRARY_PATH
fi

# podman
export DOCKER_HOST=unix://$XDG_RUNTIME_DIR/podman/podman.sock

# direnv
if command -v direnv >/dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi

# コマンドエイリアス
alias ls="ls --color=auto --group-directories-first"
alias ll="ls -lh"
alias la="ll -A"
alias vi="vim"

# 画像とPDFファイルを直接開けるように
# shellcheck disable=SC2139
alias -s {bmp,gif,jpg,jpeg,png,tiff,BMP,GIF,JPG,JPEG,PNG,TIFF}=viewnior
alias -s pdf=mupdf

# aider
if [[ -f ~/.local/bin/aider ]]; then
  path+=("$HOME/.local/bin")
  alias ai-gemini="aider --model gemini/gemini-2.5-flash"
  alias ai-claude="aider --model anthropic/claude-3-5-sonnet"
fi

# lessをカラー表示
if [[ -x /usr/bin/source-highlight-esc.sh ]];then
  export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
  export LESS='-R '
fi

# @refs https://github.com/catppuccin/skim
export SKIM_DEFAULT_OPTIONS="$SKIM_DEFAULT_OPTIONS \
--color=fg:#cdd6f4,matched:#313244,matched_bg:#f2cdcd,current:#cdd6f4,current_bg:#45475a,current_match:#1e1e2e,current_match_bg:#f5e0dc,spinner:#a6e3a1,info:#cba6f7,prompt:#89b4fa,cursor:#f38ba8,selected:#eba0ac,header:#94e2d5,border:#6c7086"

# sk(skim)でコマンド履歴を検索
sk-history () {
  BUFFER=$(history -n 1 | sk)
  CURSOR=$#BUFFER
  zle reset-prompt
}
zle -N sk-history
bindkey '^R' sk-history

# ghq + sk でリポジトリ移動
sk-ghq () {
  local selected_dir
  selected_dir=$(ghq list -p | sk --prompt="ghq > " --query "$LBUFFER")
  if [[ -n ${selected_dir} ]]; then
    BUFFER="cd ${selected_dir}"
    zle accept-line
  fi
  zle clear-screen
}
zle -N sk-ghq
bindkey '^]' sk-ghq

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-max 1000
zstyle ':chpwd:*' recent-dirs-file "$HOME/.cache/chpwd-recent-dirs"
zstyle ':chpwd:*' recent-dirs-prune 'parent'
zstyle ':completion:*' recent-dirs-insert always

# cdr + sk でディレクトリ移動
sk-cdr () {
  local selected_dir
  selected_dir=$(cdr -l | sed 's/^[0-9]\+ \+//' | sk --prompt="cdr > " --query "$LBUFFER")
  if [[ -n "${selected_dir}" ]]; then
    BUFFER="cd ${selected_dir}"
    zle accept-line
  fi
  zle clear-screen
}
zle -N sk-cdr
bindkey '^[' sk-cdr

setopt auto_param_slash      # ディレクトリ名補完で末尾のスラッシュも付与
setopt auto_param_keys       # カッコを自動補完
setopt mark_dirs             # ファイル名展開でディレクトリマッチ時は末尾スラッシュを補完
setopt auto_menu             # 補完キーで自動補完
setopt magic_equal_subst     # ロングオプションの引数も補完可能に
setopt auto_cd               # ディレクトリ名のみでcd
setopt correct               # スペルミス補正
setopt interactive_comments  # CLIでも'#'以降をコメントとみなす

# zellij
# startx 前と emacs の vterm 経由では起動しない
if [[ -z ${INSIDE_EMACS} ]] && [[ -n ${DISPLAY} ]]; then
  if command -v zellij >/dev/null 2>&1; then
    eval "$(zellij setup --generate-auto-start zsh)"
  fi
fi
