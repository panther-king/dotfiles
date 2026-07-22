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
        ./common.nix
      ];
    in
    {
      nixosConfigurations = {
        letsnote = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            disko.nixosModules.disko
            ./hosts/letsnote/configuration.nix
            ./hosts/letsnote/disko-config.nix
            {
              home-manager.users.i = {
                imports = [
                  ./desktop.nix
                  ./home.nix
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
            ./hosts/wsl2/configuration.nix
            {
              home-manager.users.i = {
                imports = [
                  ./home.nix
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
