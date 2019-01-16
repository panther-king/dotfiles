# preztoをロード
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# コマンド履歴
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=1000000

setopt HIST_IGNORE_DUPS  # 重複するコマンド履歴は無視
setopt auto_param_keys   # カッコの対応を自動補完
setopt correct           # コマンドのスペル補正
# setopt extended_glob     # glob機能の拡張
# setopt hist_ignore_dups  # 直前と同じコマンドは.zsh_historyに追加しない
# setopt list_packed       # 補完候補を詰めて表示
# setopt list_types        # 補完候補にファイル種類も表示
# setopt magic_equal_subst # オプションの"="以降も補完
# setopt nolistbeep
# setopt nonomatch
# setopt notify            # バックグラウンドジョブの状態変化を通知
setopt print_eight_bit   # 日本語ファイル名等の8ビットを通す
setopt extended_history  # コマンド実行日時も記録

# コマンドエイリアス
alias vi="vim"
alias reboot="sudo reboot"
alias halt="sudo poweroff"
alias py="python"
alias bp="bpython"
alias hp="http-prompt"
alias ocaml="rlwrap ocaml"
alias remote-desktop="remmina"

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

# autoload history-search-end
# zle -N history-beginning-search-backward-end history-search-end
# zle -N history-beginning-search-forward-end hitory-search-end
# bindkey "" history-beginning-search-backward-end
# bindkey "" history-beginning-search-forward-end

# 外部設定ファイルの読み込み
case ${UID} in
    0)
        :
        ;;
    *)
        for f in `find ~/.zsh/options -name "*.zsh" -type f`; do
            source ${f}
        done
        ;;
esac

# rust用環境変数
source $HOME/.cargo/env
export LD_LIBRARY_PATH=$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH
alias rust="cargo-script"

# anyenv初期化
export PATH=$PATH:$HOME/.anyenv/bin
eval "$(anyenv init -)"

# direnv初期化
eval "$(direnv hook zsh)"

# pip install --user 用のPATH追加
[ -d ~/.local/bin ] && export PATH=$PATH:$HOME/.local/bin

# 変数の重複を除去
typeset -U path PATH fpath FPATH

# lessをカラー表示
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS='-R '

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

# added by travis gem
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh

# OPAM configuration
. /home/taro/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
