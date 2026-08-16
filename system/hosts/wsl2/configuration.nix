{ pkgs, ... }: {
  system.stateVersion = "26.05";

  users.users.i = {
    extraGroups = [
      "wheel"
    ];
    isNormalUser = true;
    shell = pkgs.zsh;
  };
}
