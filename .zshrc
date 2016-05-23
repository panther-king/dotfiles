autoload -U compinit # 補完機能有効化
compinit

autoload -U colors
colors

# solarizedカラーテーマ
# git clone https://github.com/seebi/dircolors-solarized.git
eval $(dircolors /home/taro/github/dircolors-solarized/dircolors.ansi-dark)

# シンタックスハイライト
# yaourt -S zsh-syntax-highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

case ${UID} in # rootと一般ユーザーのIFを分ける
0)
    PROMPT="(%#^-^) "
    RPROMPT="[%F{green}%d%f] "
    RPROMPT2="[%F{green}%_%f] "
    SPROMPT="%F{yellow}%B%r is correct? [n,y,a,e]:%b%f "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{37m%}${HOST%%.*} ${PROMPT}"
    ;;
*)
    PROMPT="(%#^-^) "
    RPROMPT="[%F{green}%~%f] "
    RPROMPT2="[%F{green}%_%f] "
    SPROMPT="%F{yellow}%B%r is correct? [n,y,a,e]:%b%f "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{[37m%}${HOST%%.*} ${PROMPT}"
    ;;
esac

HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=1000000

bindkey -e # emacsキーバインド

setopt auto_cd           # cd不要でディレクトリ移動
setopt auto_list         # 補完候補の一覧表示
setopt auto_menu         # 補完候補のインクリメント表示
setopt auto_pushd        # cd時にディレクトリスタックへpushd
setopt auto_param_keys   # カッコの対応を自動補完
setopt brace_ccl
setopt correct           # コマンドのスペル補正
setopt extended_glob     # glob機能の拡張
setopt hist_ignore_dups  # 直前と同じコマンドは.zsh_historyに追加しない
setopt list_packed       # 補完候補を詰めて表示
setopt list_types        # 補完候補にファイル種類も表示
setopt magic_equal_subst # オプションの"="以降も補完
setopt nolistbeep
setopt nonomatch
setopt notify            # バックグラウンドジョブの状態変化を通知
setopt print_eight_bit   # 日本語ファイル名等の8ビットを通す
setopt share_history     # 他のシェルのヒストリを共有
setopt extended_history  # コマンド実行日時も記録

# コマンドエイリアス
alias ls="ls --color"
alias ll="ls -l"
alias la="ls -la"
alias vi="vim"
alias diff="colordiff"
alias reboot="sudo reboot"
alias halt="sudo poweroff"
alias py="python"
alias sublime="/opt/sublime-text/sublime_text"
alias top="htop"
alias bp="bpython"

# スクリプトと画像ファイルを直接実行できるように
alias -s py=python
alias -s php=php
alias -s rb=ruby
alias -s pl=perl
alias -s {bmp,gif,jpg,jpeg,png,tiff,BMP,GIF,JPG,JPEG,PNG,TIFF}=geeqie
alias -s pdf=mupdf

# gitのエイリアス
alias gad="git add -p"
alias gbr="git branch"
alias gcm="git commit -v"
alias gco="git checkout"
alias gsl="git stash list"
alias gst="git status"

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end hitory-search-end
bindkey "" history-beginning-search-backward-end
bindkey "" history-beginning-search-forward-end

# 補完機能強化
zstyle ':completion:*' completer _expand _complete _match _prefix _approximate _list _history
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
zstyle ':completion:*' list-separator '-->'
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' verbose yes
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*:descriptions' format '%F{YELLOW}completing %B%d%b'$DEFAULT
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:messages' format '%F{YELLOW}%d'$DEFAULT
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:warnings' format '%F{RED}No matches for:''%F{YELLOW} %d'$DEFAULT

synclient CircularScrolling=1
synclient CircScrollTrigger=0

# 外部設定ファイルの読み込み
case ${UID} in
1000)
    for f in `find ~/.zsh/options -name "*.zsh" -type f`; do
        source ${f}
    done
    ;;
*)
    ;;
esac

# rust用環境変数
export RUST_SRC_PATH=/usr/local/src/rustc-nightly/src
export PATH=$PATH:$HOME/.multirust/toolchains/stable/cargo/bin

# anyenv初期化
export PATH=$HOME/.anyenv/bin:$PATH
eval "$(anyenv init -)"

# direnv初期化
eval "$(direnv hook zsh)"

# pip zsh completion start
function _pip_completion {
  local words cword
  read -Ac words
  read -cn cword
  reply=( $( COMP_WORDS="$words[*]" \
             COMP_CWORD=$(( cword-1 )) \
             PIP_AUTO_COMPLETE=1 $words[1] ) )
}
compctl -K _pip_completion pip
# pip zsh completion end
