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

# コマンドエイリアス
alias vi="vim"
alias reboot="sudo reboot"
alias halt="sudo poweroff"
alias viewer="viewnior"
alias docker="sudo docker"

# スクリプトと画像ファイルを直接実行できるように
alias -s py=python
alias -s php=php
alias -s rb=ruby
alias -s pl=perl
alias -s {bmp,gif,jpg,jpeg,png,tiff,BMP,GIF,JPG,JPEG,PNG,TIFF}=viewnior
alias -s pdf=mupdf

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
. /opt/asdf-vm/asdf.sh

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
