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
        rust-nightly = pkgs.rust-bin.nightly."2024-04-28";
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            # Rust
            (lib.hiPrio rust-nightly.rustfmt)
            (lib.hiPrio rust-nightly.rust-analyzer)
            rust-bin.stable.latest.default
            gdb
            cargo-tarpaulin
          ];
        };
        devShells.nightly = pkgs.mkShell {
          packages = [
            (rust-nightly.default.override
              {
                extensions = ["rust-src" "miri-preview"];
              })
          ];
        };
      }
    );
}
