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
    dockerfile-language-server
    dos2unix
    emacs-lsp-booster
    fzf
    gh
    ghq
    git
    imagemagick
    jq
    jless
    killall
    man-pages
    man-pages-posix
    morisawa-biz-ud-gothic-fonts # overlay
    nkf
    nmap
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    opentofu
    pandoc
    peco
    ripgrep
    shellcheck
    skim
    socat # Claude Code サンドボックス用
    sourceHighlight
    tig
    tmux
    tofu-ls
    tokei
    traceroute
    tree
    udev-gothic # overlay
    unzip
    vim
    vscode-langservers-extracted
    wget
    whois
    yaml-language-server
    yazi
    zinit
    zip
  ];

  # dotfiles
  home.file.".vimrc".source = ./vim/vimrc;
  home.file.".zshrc".source = ./zsh/zshrc;

  # 環境変数
  home.sessionVariables = {
    DOCKER_HOST = "unix://\${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/podman/podman.sock";
    LESS = "-R ";
    LESSOPEN = "| ${pkgs.sourceHighlight}/bin/source-highlight-esc.sh %s";
    PATH = "$HOME/.cargo/bin:$PATH";
    SKIM_DEFAULT_OPTIONS = "--color=" + builtins.concatStringsSep "," [
      "border:#6c7086"
      "current:#cdd6f4"
      "current_bg:#45475a"
      "current_match:#1e1e2e"
      "current_match_bg:#f5e0dc"
      "cursor:#f38ba8"
      "fg:#cdd6f4"
      "header:#94e2d5"
      "info:#cba6f7"
      "matched:#313244"
      "matched_bg:#f2cdcd"
      "prompt:#89b4fa"
      "selected:#eba0ac"
      "spinner:#a6e3a1"
    ];
  };

  programs.direnv.enable = true;
  programs.mise.enable = true;

  # Alacritty
  xdg.configFile."alacritty/config.toml".source = ./xdg-config/alacritty/alacritty.toml;
  xdg.configFile."alacritty/catppuccin-mocha.toml".source = pkgs.catppuccin-alacritty;

  # bat
  xdg.configFile."bat/config".source = ./xdg-config/bat/config;
  xdg.configFile."bat/themes/Catppuccin Mocha.tmTheme".source = pkgs.catppuccin-bat;

  # emacs
  xdg.configFile."emacs/early-init.el".source = ./xdg-config/emacs/early-init.el;
  xdg.configFile."emacs/init.el".source = ./xdg-config/emacs/init.el;

  # tmux
  xdg.configFile."tmux/tmux.conf".source = ./xdg-config/tmux/tmux.conf;
  xdg.configFile."tmux/plugins/catppuccin/tmux/catppuccin.tmux".source = pkgs.catppuccin-tmux;

  # yazi
  xdg.configFile."yazi/keymap.toml".source = ./xdg-config/yazi/keymap.toml;
  xdg.configFile."yazi/theme.toml".source = pkgs.catppuccin-yazi-blue;
  xdg.configFile."yazi/Catppuccin-mocha.tmTheme".source = pkgs.catppuccin-bat;
}
