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
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    xremap-flake = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:xremap/nix-flake";
    };
  };

  outputs =
    {
      nixpkgs,
      disko,
      home-manager,
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
      };
    };
}
