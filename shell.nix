let
  nixpkgs = (import <nixpkgs> { }).fetchFromGitHub {
    name = "nixos-unstable-2020-07-09";
    owner = "nixos";
    repo = "nixpkgs";
    rev = "8d05772134f17180fb2711d0660702dae2a67313";
    sha256 = "0pnyg26c1yhnp3ymzglc71pd9j0567ymqy6il5ywc82bbm1zy25a";
  };

in { compiler ? "ghc883" }:
let
  hsOverlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages."${compiler}".override {
          overrides = haskellSelf: haskellSuper: {
            ghc = haskellSuper.ghc // {
              withPackages = haskellSuper.ghc.withHoogle;
            };
            ghcWithPackages = haskellSelf.ghc.withPackages;
          };
        };
      };
    };
  };

  orig_pkgs = import nixpkgs { };
  pkgs = import orig_pkgs.path { overlays = [ hsOverlay ]; };
  ghc = pkgs.haskell.packages."${compiler}";
  pkg = ghc.developPackage {
    root = ./.;
    source-overrides = { };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
        cabal-install
        ghcid
        haskell-language-server
        hlint
        hoogle
        ormolu
      ]);
  };
  buildInputs = [ ];

in pkg.overrideAttrs
(attrs: { buildInputs = attrs.buildInputs ++ buildInputs; })
