autoload -U compinit # 補完機能有効化
compinit

autoload -U colors
colors

export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lz=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:'

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
alias h="cd ${HOME}"

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
    source ~/.zsh/options/*.zsh
    ;;
*)
    ;;
esac

CHROME_BIN=/usr/bin/chromium

# ruby用環境変数
export PATH=$PATH:$HOME/.gem/ruby/2.1.0/bin

# golang用環境変数
export GOROOT=/usr/lib/go
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
