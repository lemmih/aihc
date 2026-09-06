{
  description = "aihc development flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    root = ./.;
    core = import ./scripts/nix/core.nix {inherit nixpkgs;};
    sources = import ./scripts/nix/sources.nix {inherit root;};
    haskell = import ./scripts/nix/haskell-packages.nix {
      inherit (core) projectHsPackages;
      inherit sources;
    };
    docs = import ./scripts/nix/docs.nix {};
    mkWasiSysroot = import ./scripts/nix/wasi-sysroot.nix;
    mkPackages = import ./scripts/nix/packages.nix {
      inherit (docs) mkUserGuide;
    };
    mkApps = import ./scripts/nix/apps.nix {
      inherit (core) projectHsPackages;
      inherit (haskell) mkHsPkgs;
    };
    mkChecks = import ./scripts/nix/checks.nix {
      inherit (core) projectHsPackages;
      inherit sources mkWasiSysroot;
      inherit (haskell) mkHsPkgsForChecks;
    };
    mkDevShells = import ./scripts/nix/dev-shells.nix {
      inherit mkWasiSysroot;
      inherit (haskell) mkHsPkgs;
    };
  in {
    packages = core.forAllSystems (pkgs: mkPackages pkgs // (mkChecks pkgs).packages);

    formatter = core.forAllSystems (pkgs: pkgs.alejandra);

    apps = core.forAllSystems mkApps;

    checks = core.forAllSystems (pkgs: (mkChecks pkgs).checks);

    devShells = core.forAllSystems mkDevShells;
  };
}
