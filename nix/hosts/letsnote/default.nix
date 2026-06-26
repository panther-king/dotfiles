{ ... }: {
  imports = [ ./hardware-configuration.nix ];

  # systemd でブートする
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 5;
  boot.loader.systemd-boot.enable = true;

  networking.hostName = "stfuawsc";
  networking.networkmanager.enable = true;
}
