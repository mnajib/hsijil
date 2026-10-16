{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = [
    (pkgs.haskellPackages.ghcWithPackages (p: [
      p.cairo p.cassava p.vector p.containers p.text
      p.bytestring p.directory p.filepath
    ]))
    pkgs.cairo
    pkgs.pkg-config
    pkgs.gnumake      # untuk `make`
    pkgs.fontconfig   # untuk `fc-list` (semak nama font)
  ];
}
