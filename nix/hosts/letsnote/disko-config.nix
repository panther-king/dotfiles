{
  disko.devices = {
    disk = {
      nvme0n1 = {
        content = {
          partitions = {
            ESP = {
              content = {
                format = "vfat";
                mountOptions = [
                  "umask=0077"
                ];
                mountpoint = "/boot";
                type = "filesystem";
              };
              name = "ESP";
              size = "1G";
              type = "EF00";
            };
            root = {
              content = {
                extraArgs = [
                  "-f"
                ];
                subvolumes = {
                  "/home" = {
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                    mountpoint = "/home";
                  };
                  "/nix" = {
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                    mountpoint = "/nix";
                  };
                  "/root" = {
                    mountOptions = [
                      "compress=zstd"
                      "noatime"
                    ];
                    mountpoint = "/";
                  };
                };
                type = "btrfs";
              };
              size = "100%";
            };
          };
          type = "gpt";
        };
        device = "/dev/nvme0n1";
        type = "disk";
      };
    };
  };
}
