let
  reflex-platform = import ./reflex-platform {};
in
reflex-platform.project ({ pkgs, ... }: {
  packages = {
    game-3d = ./.;
  };

  shells = {
    ghc = ["game-3d"];
  };

  overrides = self: super: {
    reflex-gloss = import (pkgs.fetchFromGitHub {
      owner = "lightandlight";
      repo = "reflex-gloss";
      rev = "02dff0b50e0c2500ef447c82f5ddd0736f85c173";
      sha256 = "0w8j3wrad0l284ajd54ayal5rmj6d0v6q6hhk7mmssq7448y3j37";
    }) { inherit reflex-platform; };
  };
})