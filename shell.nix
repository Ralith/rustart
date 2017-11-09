with import <nixpkgs> { };
stdenv.mkDerivation {
  name = "rustart";
  buildInputs = with pkgs; [ rust-nightly ];
  shellHook = ''
    export CARGO_INCREMENTAL=1
  '';
}
