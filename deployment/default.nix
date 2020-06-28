with import ../nix;

let
  # Local packages.
  packages = import ../release.nix;

  server = haskell.lib.justStaticExecutables packages.csdc-api;
  gui = packages.csdc-gui;

  # Server configuration.
  writeJson = name: tree:
    writeText name (builtins.toJSON tree);

  config = writeJson "csdc-dao-config" {
    port = 8080;
    path = gui;
    orcidEndpoint = "production";
  };
in
  # We are using buildImage instead of buildLayeredImage because of the
  # runAsRoot option. The Haskell http-client library is not aware of the
  # SSL_SECRET_FILE environment variable, so we have to copy certificates to
  # where it expects them to be, which can only be done as root.
  dockerTools.buildImage {
    name = "csdc-dao";
    tag = "latest";
    contents = [
      server
      busybox
    ];
    config = {
      Cmd = [
        "/bin/csdc-server" config
      ];
      ExposedPorts = {
        "8080" = {};
      };
    };
    runAsRoot = ''
      mkdir -p /etc/ssl/certs
      ln -s ${cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt
    '';
  }
