{
  description = "NixOS cconfiguration";

  inputs = {
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
            ./hosts/letsnote
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
