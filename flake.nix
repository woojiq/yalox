{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system}.extend (import rust-overlay);
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            # Rust
            rust-bin.stable.latest.default
            rust-analyzer
            gdb
            cargo-tarpaulin
          ];
        };
        devShells.nightly = pkgs.mkShell {
          packages = with pkgs; [
            rust-bin.nightly."2024-02-01".default
          ];
        };
      }
    );
}
