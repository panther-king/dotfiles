{
  description = "NixOS configuration";

  inputs = {
    disko = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/disko/latest";
    };
    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager";
    };
    nixos-wsl = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/NixOS-WSL/main";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    xremap-flake = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:xremap/nix-flake";
    };
  };

  outputs =
    {
      disko,
      home-manager,
      nixos-wsl,
      nixpkgs,
      xremap-flake,
      ...
    }:
    let
      commonModules = [
        home-manager.nixosModules.home-manager
        ./system/common.nix
      ];
    in
    {
      nixosConfigurations = {
        letsnote = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            disko.nixosModules.disko
            ./system/hosts/letsnote/configuration.nix
            ./system/hosts/letsnote/disko-config.nix
            {
              home-manager.users.i = {
                imports = [
                  ./home/common.nix
                  ./home/hosts/letsnote.nix
                  xremap-flake.homeManagerModules.default
                ];
              };
            }
          ];
        };
        wsl2 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            nixos-wsl.nixosModules.default
            ./system/hosts/wsl2/configuration.nix
            {
              home-manager.users.i = {
                imports = [
                  ./home/common.nix
                ];
              };
              wsl.defaultUser = "i";
              wsl.docker-desktop.enable = true;
              wsl.enable = true;
            }
          ];
        };
      };
    };
}
