# 公式インストーラー経由の cargo
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

# docker ではなく podman を利用
export DOCKER_HOST="unix://${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/podman/podman.sock"

# lessをカラー表示
if [[ -x /usr/bin/source-highlight-esc.sh ]];then
  export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
  export LESS='-R '
fi

# skim の catppuccin テーマ
# @refs https://github.com/catppuccin/skim
export SKIM_DEFAULT_OPTIONS="$SKIM_DEFAULT_OPTIONS \
--color=fg:#cdd6f4,matched:#313244,matched_bg:#f2cdcd,current:#cdd6f4,current_bg:#45475a,current_match:#1e1e2e,current_match_bg:#f5e0dc,spinner:#a6e3a1,info:#cba6f7,prompt:#89b4fa,cursor:#f38ba8,selected:#eba0ac,header:#94e2d5,border:#6c7086"

# xmonad で Java GUI アプリケーションが
# 期待どおりの挙動にならない問題の対策
# @refs https://wiki.archlinux.jp/index.php/Xmonad
export _JAVA_AWT_WM_NONREPARENTING=1
