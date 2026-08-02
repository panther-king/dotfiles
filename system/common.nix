{ pkgs, ... }: {
  # zsh のプラグインを .zshrc でロードできるようにしておく
  environment.variables = {
    ZSH_AUTOSUGGESTIONS = "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh";
    ZSH_FAST_SYNTAX_HIGHLIGHTING = "${pkgs.zsh-fast-syntax-highlighting}/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh";
    ZSH_FZF_TAB = "${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh";
  };

  # home-manager を効率的に利用する
  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  # ja_JP.UTF-8 はユーザーセッションで上書きする
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.supportedLocales = [
    "en_US.UTF-8/UTF-8"
    "ja_JP.UTF-8/UTF-8"
  ];

  # 過去バージョンはこまめに削除する
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 10d";
  };

  # 最新のメジャー機能を利用できるようにしておく
  nix.settings.experimental-features = [
    "flakes"
    "nix-command"
  ];

  # フリーではないパッケージや自前の overlay も利用する
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    (import ./overlays/catppuccin.nix)
    (import ./overlays/morisawa-biz-ud-gothic-fonts.nix)
  ];

  # シェルは zsh
  programs.zsh.enable = true;

  # 日本以外では利用しない
  time.timeZone = "Asia/Tokyo";

  # docker ではなく podman を利用する
  virtualisation.podman = {
    # docker コマンドを podman にエイリアス
    dockerCompat = true;
    enable = true;
  };
  # podman compose 時の警告を無効化
  virtualisation.containers.containersConf.settings = {
    engine.compose_warning_logs = false;
  };
  # ホスト未指定のイメージは docker.io を見に行く
  virtualisation.containers.registries.settings = {
    unqualified-search-registries = [
      "docker.io"
    ];
  };
}
