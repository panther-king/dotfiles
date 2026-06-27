{ pkgs, ... }: {
  home.homeDirectory = "/home/i";
  home.username = "i";
  home.stateVersion = "26.05";

  # ホストを問わず利用するパッケージ
  home.packages = with pkgs; [
    alacritty
    bash-language-server
    bat
    curl
    delta
    devcontainer
    diff-so-fancy
    difftastic
    direnv
    dockerfile-language-server
    dos2unix
    emacs-lsp-booster
    gh
    ghq
    git
    imagemagick
    jq
    jless
    killall
    man-pages
    man-pages-posix
    morisawa-biz-ud-gothic-fonts  # overlay
    nkf
    nmap
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    opentofu
    pandoc
    peco
    shellcheck
    skim
    socat # Claude Code サンドボックス用
    sourceHighlight
    starship
    tig
    tmux
    tofu-ls
    tokei
    traceroute
    tree
    udev-gothic  # overlay
    unzip
    vim
    vscode-langservers-extracted
    wget
    whois
    yaml-language-server
    yazi
    zip
  ];
}
