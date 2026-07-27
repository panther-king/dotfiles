{ pkgs, ... }: {
  system.stateVersion = "26.05";

  # mise でインストールしたソフトウェア用が
  # リンカや共有ライブラリを参照できるようにする
  programs.nix-ld.enable = true;

  users.users.i = {
    extraGroups = [
      "wheel"
    ];
    isNormalUser = true;
    shell = pkgs.zsh;
  };
}
