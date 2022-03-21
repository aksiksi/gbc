# Nix development environment (Linux or macOS)
#
# To setup a shell: `nix-shell [shell.nix]`
# To cleanup the Nix store after using the shell: `nix-collect-garbage`
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/64fc73bd74f04d3e10cb4e70e1c65b92337e76db.tar.gz") {}
}:

pkgs.mkShell {
  buildInputs = [
    pkgs.rustc
    pkgs.cargo
    pkgs.openssl
    pkgs.cmake
    pkgs.curl
    pkgs.libiconv
    pkgs.pkg-config
  ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
    # nixpkgs/pkgs/os-specific/darwin/apple-sdk/frameworks.nix
    pkgs.darwin.apple_sdk.frameworks.AudioToolbox
    pkgs.darwin.apple_sdk.frameworks.Cocoa
    pkgs.darwin.apple_sdk.frameworks.ForceFeedback
    pkgs.darwin.apple_sdk.frameworks.Foundation
    pkgs.darwin.apple_sdk.frameworks.OpenGL
    pkgs.darwin.apple_sdk.frameworks.Security
  ];
}

