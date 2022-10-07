let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
  }) {};

  # To update to a newer version of easy-purescript-nix:
  # 1. Obtain the commit hash <rev> via `curl https://api.github.com/repos/justinwoo/easy-purescript-nix/commits/master`.
  # 2. Obtain the sha256 hash <sha256> via `nix-prefetch-url --unpack https://github.com/justinwoo/easy-purescript-nix/archive/<rev>.tar.gz`.
  # 3. Update the <rev> and <sha256> below.
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "ee51a6d459b8fecfcb10f24ca9728e649c6a9e00";
    sha256 = "1w4k6mcayyg6388na0cca5qx94sm99xn3na26x2w34jlyz3bwl3m";
  }) {inherit pkgs;};
in
  pkgs.stdenv.mkDerivation {
    name = "tecton";
    buildInputs = with pursPkgs; [
      purs
      spago
      pulp
      purs-tidy

      pkgs.nodejs-16_x
      pkgs.nodePackages.bower
    ];
  }
