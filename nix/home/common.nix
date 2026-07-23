{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.my.emacs;
  fsharpSignatureGrammar = pkgs.tree-sitter.buildGrammar {
    # fsharp-ts-mode が要求する .fsi 用の tree-sitter が nixpkgs に無いため、
    # tree-sitter-fsharp のソース内にある別ディレクトリを流用する
    language = "fsharp-signature";
    inherit (pkgs.tree-sitter-grammars.tree-sitter-fsharp) version src;
    location = "fsharp_signature";
  };
  treesitGrammars = cfg.package.pkgs.treesit-grammars.with-grammars (
    grammars:
    with grammars;
    [
      tree-sitter-bash
      tree-sitter-css
      tree-sitter-dockerfile
      tree-sitter-fsharp
      tree-sitter-haskell
      tree-sitter-html
      tree-sitter-javascript
      tree-sitter-jsdoc
      tree-sitter-json
      tree-sitter-kdl
      tree-sitter-mermaid
      tree-sitter-nix
      tree-sitter-php
      tree-sitter-phpdoc
      tree-sitter-python
      tree-sitter-rust
      tree-sitter-toml
      tree-sitter-tsx
      tree-sitter-typescript
    ]
    ++ [
      fsharpSignatureGrammar
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
  # emacs は Wayland on Let's note と
  # X11 on WSL2 で異なるパッケージを使えるようにしておく
  options.my.emacs.package = lib.mkOption {
    type = lib.types.package;
    default = pkgs.emacs31;
  };

  config = {
    home.homeDirectory = "/home/i";
    home.username = "i";
    home.stateVersion = "26.05";

    # ホストを問わず利用するパッケージ
    home.packages =
      with pkgs;
      [
        alacritty
        awscli2
        bash-language-server
        bat
        claude-code
        curl
        d2
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
        gnumake
        graphviz
        imagemagick
        jq
        jless
        killall
        man-pages
        man-pages-posix
        mermaid-cli
        mise
        morisawa-biz-ud-gothic-fonts # overlay
        nh # nix cli helper
        nixd # nix LSP
        nixfmt
        nkf
        nmap
        noto-fonts-cjk-sans
        noto-fonts-cjk-serif
        noto-fonts-color-emoji
        pandoc
        pure-prompt # zsh prompt
        peco
        plantuml
        ripgrep
        shellcheck
        skim
        socat # Claude Code サンドボックス用
        sourceHighlight
        taplo # TOML LSP
        tig
        tmux
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
      ]
      ++ [ cfg.package ];

    # dotfiles
    home.file.".vimrc".source = ./config/vim/vimrc;

    home.sessionPath = [
      "$HOME/.cargo/bin"
    ];

    # 環境変数
    home.sessionVariables = {
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

    programs.bat = {
      config.theme = "Catppuccin Mocha";
      enable = true;
      themes."Catppuccin Mocha".src = pkgs.catppuccin-bat;
    };

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      mise.enable = true;
      nix-direnv.enable = true;
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
        include.path = [
          "~/.config/git/local"
          "${pkgs.catppuccin-delta}"
        ];
      };
    };

    programs.zsh = {
      enable = true;
      # 保管設定は .zshrc 内の定義に任せる
      enableCompletion = false;
      initContent = builtins.readFile ./config/zsh/zshrc;
    };

    # Alacritty
    xdg.configFile."alacritty/alacritty.toml".source = ./config/alacritty/alacritty.toml;
    xdg.configFile."alacritty/catppuccin-mocha.toml".source = pkgs.catppuccin-alacritty;

    # emacs
    xdg.configFile."emacs/early-init.el".source = ./config/emacs/early-init.el;
    xdg.configFile."emacs/init.el".source = ./config/emacs/init.el;
    xdg.configFile."emacs/treesit-grammars".source = "${treesitGrammars}/lib";
    xdg.dataFile."skk/dictionaries".source = "${skkDictionaries}/share/skk";

    # tmux
    xdg.configFile."tmux/tmux.conf".source = ./config/tmux/tmux.conf;
    xdg.configFile."tmux/plugins/catppuccin/tmux".source = pkgs.catppuccin-tmux;

    # yazi
    xdg.configFile."yazi/keymap.toml".source = ./config/yazi/keymap.toml;
    xdg.configFile."yazi/theme.toml".source = pkgs.catppuccin-yazi-blue;
    xdg.configFile."yazi/Catppuccin-mocha.tmTheme".source = pkgs.catppuccin-bat;
  };
}
