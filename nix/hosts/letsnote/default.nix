{ pkgs, ... }: {
  imports = [ ./hardware-configuration.nix ];

  # systemd でブートする
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 5;
  boot.loader.systemd-boot.enable = true;

  # GRUB ではなく UEFI でブートする
  environment.systemPackages = with pkgs; [
    efibootmgr
  ];

  networking.hostName = "stfuawsc";
  networking.networkmanager.enable = true;

  programs.dconf.enable = true;

  # ALSA でオーディオを管理する
  security.rtkit.enable = true;
  services.pipewire = {
    alsa.enable = true;
    enable = true;
    pulse.enable = true;
  };

  # greetd/tuigreet でログインして niri を起動する
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet --cmd 'zsh -l -c niri-session' --time --remember --issue";
        user = "greeter";
      };
      terminal = {
        vt = 1;
      };
    };
  };

  # docker ではなく podman を利用する
  virtualisation.podman = {
    # docker コマンドを podman にエイリアス
    dockerCompat = true;
    enable = true;
  };
}
