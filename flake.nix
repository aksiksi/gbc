{
  description = "A Gameboy Color (GBC) emulator written in Rust.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs, ... }: let
    supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    pname = "gbc";
    owner = "aksiksi";
    version = "0.1.0";
  in {
    packages = forAllSystems (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        default = pkgs.rustPlatform.buildRustPackage {
          inherit pname;
          inherit version;
          src = ./.;
          cargoSha256 = "sha256-BWgLuuPlecgjoI3z5Uqw1Djek2FHsW9REo6PdiL2z2A=";
          # Builds a crate within a Cargo workspace.
          buildAndTestSubdir = "emu/";
          buildNoDefaultFeatures = true;
          nativeBuildInputs = [
            pkgs.cmake
            pkgs.pkg-config
            pkgs.SDL2
          ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.darwin.apple_sdk.frameworks.Cocoa
            pkgs.darwin.apple_sdk.frameworks.ForceFeedback
            pkgs.darwin.apple_sdk.frameworks.Foundation
            pkgs.darwin.apple_sdk.frameworks.OpenGL
            pkgs.darwin.apple_sdk.frameworks.Security
          ];
          buildInputs = [
            pkgs.SDL2
          ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.darwin.apple_sdk.frameworks.Cocoa
            pkgs.darwin.apple_sdk.frameworks.ForceFeedback
            pkgs.darwin.apple_sdk.frameworks.Foundation
            pkgs.darwin.apple_sdk.frameworks.OpenGL
            pkgs.darwin.apple_sdk.frameworks.Security
          ];
          meta = {
            description = "A Gameboy Color (GBC) emulator written in Rust.";
            homepage = "https://github.com/aksiksi/gbc";
            license = [];
            maintainers = [];
          };
        };
      }
    );

    # Development shell
    # nix develop
    devShells = forAllSystems (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        # Build:
        # cargo build --no-default-features
        default = pkgs.mkShell {
          buildInputs = [
            pkgs.libiconv
            pkgs.SDL2
          ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.darwin.apple_sdk.frameworks.Cocoa
            pkgs.darwin.apple_sdk.frameworks.ForceFeedback
            pkgs.darwin.apple_sdk.frameworks.Foundation
            pkgs.darwin.apple_sdk.frameworks.OpenGL
            pkgs.darwin.apple_sdk.frameworks.Security
          ];
          packages = [
            pkgs.cargo
            pkgs.cmake
            pkgs.pkg-config
            pkgs.rustc
            pkgs.rust-analyzer
            pkgs.rustfmt
          ];
        };
      }
    );
  };
}

