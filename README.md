# Miso @ MuniHac 2020

## Prepare the build

1. [Install Nix](https://nixos.org/download.html)
    * If you are using macOS 10.15 or newer, check the [specific instructions](https://nixos.org/manual/nix/stable/#sect-macos-installation)
2. Optionally set up a binary cache:
    1. [Install Cachix](https://docs.cachix.org/installation.html#installation): `nix-env -iA cachix -f https://cachix.org/api/v1/install`
    2. Use the Miso cache: `cachix use miso-haskell`

## Build and run the project

Go to any of the folders containing a Miso app and then do:

```sh
nix-build
open ./result/bin/miso-app.jsexe/index.html
```