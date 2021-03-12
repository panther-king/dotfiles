# preztoをロード
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# コマンド履歴
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=1000000

# autosuggestの補完色
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=242"

# setopt HIST_IGNORE_DUPS  # 重複するコマンド履歴は無視
# setopt HIST_SAVE_NO_DUPS
# setopt HIST_VERIFY
# setopt AUTO_PARAM_KEYS   # カッコの対応を自動補完
# setopt CORRECT           # コマンドのスペル補正
# setopt extended_glob     # glob機能の拡張
# setopt hist_ignore_dups  # 直前と同じコマンドは.zsh_historyに追加しない
# setopt list_packed       # 補完候補を詰めて表示
# setopt list_types        # 補完候補にファイル種類も表示
# setopt magic_equal_subst # オプションの"="以降も補完
# setopt nolistbeep
# setopt nonomatch
# setopt notify            # バックグラウンドジョブの状態変化を通知
# setopt PRINT_EIGHT_BIT   # 日本語ファイル名等の8ビットを通す
# setopt EXTENDED_HISTORY  # コマンド実行日時も記録
# setopt COMBINING_CHARS
# setopt SHARE_HISTORY

# コマンドエイリアス
alias vi="vim"
alias reboot="sudo reboot"
alias halt="sudo poweroff"

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

# asdf
. $HOME/.asdf/asdf.sh

# anyenv初期化
export PATH=$PATH:$HOME/.anyenv/bin
eval "$(anyenv init -)"

# direnv初期化
eval "$(direnv hook zsh)"

# dotnetコマンドパス
[ -d ~/.dotnet ] && export PATH=$PATH:$HOME/.dotnet

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

# starship
eval "$(starship init zsh)"
