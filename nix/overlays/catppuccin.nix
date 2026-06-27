final: prev: {
  catppuccin-alacritty = prev.fetchurl {
    hash = "sha256-lJzvF8PsUEILgscLIuOqLqCl0n38wTKEGSvVKEtdssU=";
    url = "https://raw.githubusercontent.com/catppuccin/alacritty/f6cb5a5c2b404cdaceaff193b9c52317f62c62f7/catppuccin-mocha.toml";
  };

  catppuccin-bat = prev.fetchurl {
    hash = "sha256-OVVm8IzrMBuTa5HAd2kO+U9662UbEhVT8gHJnCvUqnc=";
    url = "https://raw.githubusercontent.com/catppuccin/bat/6810349b28055dce54076712fc05fc68da4b8ec0/themes/Catppuccin%20Mocha.tmTheme";
  };

  catppuccin-fuzzel = prev.fetchurl {
    hash = "sha256-40t+WW7UMnjhvtWJS21gtbiNEEhTtDSEMoU8Wb5lPmM=";
    url = "https://raw.githubusercontent.com/catppuccin/fuzzel/879879da8a7dc58f173b4cd7987723fd19bef6d5/themes/catppuccin-mocha/blue.ini";
  };
}
