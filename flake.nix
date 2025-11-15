{
  description = "elm-uri-template is an implementation of RFC 6570";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    # Until https://github.com/NixOS/nixpkgs/pull/414495
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    allow-import-from-derivation = true; # my default, but useful for others
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = (
          import nixpkgs {
            inherit system;
          }
        );

        runDeps = with pkgs; [
          openssl
        ];

        buildDeps =
          with pkgs;
          [
            pkg-config
          ]
          ++ runDeps;
      in
      {
        devShells.default =
          let
            unstable-pkgs = (
              import nixpkgs-unstable {
                inherit system;
              }
            );

            elm-pkgs = unstable-pkgs.elmPackages;
          in
          # if you don't what to use Nix, here are the dependencies you need:
          pkgs.mkShell {
            buildInputs =
              with pkgs;
              [
                nodejs_latest
                elm-pkgs.elm
                elm-pkgs.elm-test-rs
                elm-pkgs.elm-live
                elm-pkgs.elm-review
                elm-pkgs.elm-format
                elm-pkgs.elm-doc-preview
                lightningcss
                elm2nix

                process-compose
                watchexec
              ]
              ++ buildDeps; # If you're doing your own installs, you can ignore this
          };
      }
    );
}
