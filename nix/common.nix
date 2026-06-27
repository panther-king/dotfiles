{ ... }: {
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
    (import ./overlays/morisawa-biz-ud-gothic-fonts.nix)
    (import ./overlays/udev-gothic.nix)
    (import ./overlays/waynaptics.nix)
  ];

  # シェルは zsh
  programs.zsh.enable = true;

  # 日本以外では利用しない
  time.timeZone = "Asia/Tokyo";
}
