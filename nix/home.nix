{ pkgs, ... }:
let
  treesitGrammars = pkgs.emacs31-pgtk.pkgs.treesit-grammars.with-grammars (
    grammars: with grammars; [
      tree-sitter-bash
      tree-sitter-css
      tree-sitter-dockerfile
      tree-sitter-haskell
      tree-sitter-html
      tree-sitter-javascript
      tree-sitter-jsdoc
      tree-sitter-json
      tree-sitter-kdl
      tree-sitter-nix
      tree-sitter-php
      tree-sitter-phpdoc
      tree-sitter-python
      tree-sitter-rust
      tree-sitter-toml
      tree-sitter-tsx
      tree-sitter-typescript
    ]
  );
  skkDictionaries = pkgs.symlinkJoin {
    # SKK 辞書は ddskk/fcitx5 で同じものを参照する
    # nixpkgs が UTF-8 化をサポートしているため利用する
    name = "skk-dictionaries";
    paths = map (d: d.override { useUtf8 = true; }) (
      with pkgs.skkDictionaries;
      [
        assoc
        emoji
        fullname
        geo
        itaiji
        itaiji_jis3_4
        jinmei
        jis2
        jis2004
        l
        law
        mazegaki
        station
        zipcode
      ]
    );
  };
in
{
  home.homeDirectory = "/home/i";
  home.username = "i";
  home.stateVersion = "26.05";

  # ホストを問わず利用するパッケージ
  home.packages = with pkgs; [
    alacritty
    bash-language-server
    bat
    claude-code
    curl
    delta
    devcontainer
    diff-so-fancy
    difftastic
    dockerfile-language-server
    dos2unix
    emacs31-pgtk
    emacs-lsp-booster
    fsautocomplete
    fzf
    gh
    ghq
    git
    imagemagick
    intelephense # PHP LSP
    jq
    jless
    killall
    man-pages
    man-pages-posix
    mise
    morisawa-biz-ud-gothic-fonts # overlay
    nixd # nix LSP
    nixfmt
    nkf
    nmap
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    opentofu
    pandoc
    pure-prompt # zsh prompt
    peco
    ripgrep
    shellcheck
    skim
    socat # Claude Code サンドボックス用
    sourceHighlight
    taplo # TOML LSP
    tig
    tmux
    tofu-ls
    tokei
    traceroute
    tree
    typescript-language-server
    udev-gothic-nf
    unzip
    vim
    vscode-langservers-extracted
    wget
    whois
    yaml-language-server
    yazi
    zip
    zsh-autosuggestions
    zsh-completions
    zsh-fast-syntax-highlighting
    zsh-fzf-tab
  ];

  # dotfiles
  home.file.".vimrc".source = ./vim/vimrc;

  home.sessionPath = [
    "$HOME/.cargo/bin"
  ];

  # 環境変数
  home.sessionVariables = {
    DOCKER_HOST = "unix://\${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/podman/podman.sock";
    LANG = "ja_JP.UTF-8";
    # 照合順序は常にバイト単位
    LC_COLLATE = "C";
    LESS = "-R ";
    LESSOPEN = "| ${pkgs.sourceHighlight}/bin/source-highlight-esc.sh %s";
    SKIM_DEFAULT_OPTIONS =
      "--color="
      + builtins.concatStringsSep "," [
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

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    mise.enable = true;
  };

  programs.git = {
    enable = true;
    settings = {
      credential."https://github.com".helper = [
        ""
        "${pkgs.gh}/bin/gh auth git-credential"
      ];
      credential."https://gist.github.com".helper = [
        ""
        "${pkgs.gh}/bin/gh auth git-credential"
      ];
      core = {
        pager = "delta";
        quotepath = false;
      };
      delta = {
        features = "catppuccin-mocha";
        navigate = true;
      };
      diff = {
        algorithm = "histogram";
        colorMoved = "dimmed-zebra";
      };
      ghq.root = "~/ghq";
      github.user = "panther-king";
      include.path = "${pkgs.catppuccin-delta}";
    };
  };

  programs.zsh = {
    enable = true;
    # 保管設定は .zshrc 内の定義に任せる
    enableCompletion = false;
    initContent = builtins.readFile ./zsh/zshrc;
  };

  # Alacritty
  xdg.configFile."alacritty/alacritty.toml".source = ./xdg-config/alacritty/alacritty.toml;
  xdg.configFile."alacritty/catppuccin-mocha.toml".source = pkgs.catppuccin-alacritty;

  # bat
  xdg.configFile."bat/config".source = ./xdg-config/bat/config;
  xdg.configFile."bat/themes/Catppuccin Mocha.tmTheme".source = pkgs.catppuccin-bat;

  # emacs
  xdg.configFile."emacs/early-init.el".source = ./xdg-config/emacs/early-init.el;
  xdg.configFile."emacs/init.el".source = ./xdg-config/emacs/init.el;
  xdg.configFile."emacs/treesit-grammars".source = "${treesitGrammars}/lib";
  xdg.dataFile."skk/dictionaries".source = "${skkDictionaries}/share/skk";

  # tmux
  xdg.configFile."tmux/tmux.conf".source = ./xdg-config/tmux/tmux.conf;
  xdg.configFile."tmux/plugins/catppuccin/tmux".source = pkgs.catppuccin-tmux;

  # yazi
  xdg.configFile."yazi/keymap.toml".source = ./xdg-config/yazi/keymap.toml;
  xdg.configFile."yazi/theme.toml".source = pkgs.catppuccin-yazi-blue;
  xdg.configFile."yazi/Catppuccin-mocha.tmTheme".source = pkgs.catppuccin-bat;
}
