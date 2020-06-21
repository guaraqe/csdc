# CSDC DAO

![build](https://github.com/guaraqe/csdc/workflows/build/badge.svg)

## Development

This projects uses [Nix](https://nixos.org/download.html) to manage
dependencies. To install Nix, run:

```
curl -L https://nixos.org/nix/install | sh
```

Then, enter the development shell with:

```
nix-shell
```

which will download all necessary dependencies. Most common commands are
accessible from the Makefile. To list the available commands, run `make`.

In particular:

  - For faster Haskell development, there are many `ghcid-*` targerts.

  - For Elm development, there is `make gui-build`.

## Running the server

First, make sure the GUI is built with:

```
make gui-build
```

Second, you will need to obtain an ID and a secret from
[ORCID](https://orcid.org/developer-tools), and write them to the `secret.json`
file, which should follow the model of the `secret-model.json` file.

Finally, run:

```
make serve
```

and the server should be available at `localhost:8080`.
