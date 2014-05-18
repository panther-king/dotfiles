autoload -U compinit # 補完機能有効化
compinit

autoload -U colors
colors

# solarizedカラーテーマ
# git clone https://github.com/seebi/dircolors-solarized.git
eval $(dircolors /home/taro/github/dircolors-solarized/dircolors.ansi-dark)


case ${UID} in # rootと一般ユーザーのIFを分ける
0)
    PROMPT="%m:%n%# "
    RPROMPT="[%B%{[36m%}%/%{[m%}%b] "
    RPROMPT2="[%B%{[36m%}%_%{[m%}%b] "
    SPROMPT="%B%{[36m%}%r is correct? [n,y,a,e]:%{[m%}%b "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{37m%}${HOST%%.*} ${PROMPT}"
    ;;
*)
    PROMPT="%m:%n%% "
    RPROMPT="[%{[36m%}%/%{[m%}] "
    RPROMPT2="%{[36m%}%_%{[m%} "
    SPROMPT="%{[36m%}%r is correct? [n,y,a,e]:%{[m%} "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{[37m%}${HOST%%.*} ${PROMPT}"
    ;;
esac

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

bindkey -e # emacsキーバインド

setopt auto_cd           # cd不要でディレクトリ移動
setopt auto_list         # 補完候補の一覧表示
setopt auto_menu         # 補完候補のインクリメント表示
setopt auto_pushd        # cd時にディレクトリスタックへpushd
setopt auto_param_keys   # カッコの対応を自動補完
setopt brace_ccl
setopt correct           # コマンドのスペル補正
setopt equals            # =commandで`which command`と同様の処理
setopt extended_glob     # glob機能の拡張
setopt hist_ignore_dups  # 直前と同じコマンドは.zsh_historyに追加しない
setopt list_packed       # 補完候補を詰めて表示
setopt list_types        # 補完候補にファイル種類も表示
setopt magic_equal_subst # オプションの"="以降も補完
setopt nolistbeep
setopt nonomatch
setopt notify            # バックグラウンドジョブの状態変化を通知
setopt share_history     # 他のシェルのヒストリを共有

alias ls="ls --color"
alias ll="ls -l"
alias la="ls -la"
alias vi="vim"
alias diff="colordiff"
alias reboot="sudo reboot"
alias halt="sudo poweroff"
alias irb="pry"

alias -s py=python
alias -s php=php
alias -s rb=ruby
alias -s pl=perl
alias -s {bmp,gif,jpg,png,BMP,GIF,JPG,PNG}=eog

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end hitory-search-end
bindkey "" history-beginning-search-backward-end
bindkey "" history-beginning-search-forward-end

# 補完機能強化
zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
zstyle ':completion:*' verbose yes
zstyle ':completion:*' completer _expand _complete _match _prefix _approximate _list _history
zstyle ':completion:*:messages' format '%F{YELLOW}%d'$DEFAULT
zstyle ':completion:*:warnings' format '%F{RED}No matches for:''%F{YELLOW} %d'$DEFAULT
zstyle ':completion:*:descriptions' format '%F{YELLOW}completing %B%d%b'$DEFAULT
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-separator '-->'
zstyle ':completion:*:manuals' separate-sections true

synclient CircularScrolling=1
synclient CircScrollTrigger=0

# 外部設定ファイルの読み込み
source ~/.zsh/plugin/incr*.zsh
case ${UID} in
1000)
    for f in `find ~/.zsh/options -name "*.zsh" -type f`; do
        source ${f}
    done
    ;;
*)
    ;;
esac

CHROME_BIN=/usr/bin/chromium

# virtualenvwrapper & pythonz
source `which virtualenvwrapper.sh`
[[ -s $HOME/.pythonz/etc/bashrc ]] && source $HOME/.pythonz/etc/bashrc

# ruby用環境変数
export PATH=$PATH:$HOME/.gem/ruby/2.1.0/bin

# npm用環境変数
export NODE_PATH=/usr/lib/node_modules/

# golang用環境変数
export GOROOT=/usr/lib/go
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
