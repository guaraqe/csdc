with import ../nix;

let
  # Local packages.
  packages = import ../release.nix;

  secrets = pkgs.lib.importJSON ../secrets.json;

  server = haskell.lib.justStaticExecutables packages.csdc-api;
  gui = packages.csdc-gui;

  # Server configuration.
  writeJson = name: tree:
    writeText name (builtins.toJSON tree);

  migrations = ../database/migrations;

  config = writeJson "csdc-dao-config" {
    port = 8080;
    path = gui;
    sql = {
      tag = "SQLConfigEnv";
      contents = "DATABASE_URL";
    };
    mail = {
      tag = "MailConfigEnv";
      contents = "GMAIL";
    };
    ipfs = {
      path = "";
    };
    migration = migrations;
  };

  prepareDatabaseScript = ''
    echo "Creating the database if necessary..."
    ${postgresql}/bin/psql $DATABASE_URL/postgres -tc "SELECT 1 FROM pg_database WHERE datname = 'csdc'" |
      ${gnugrep}/bin/grep -q 1 ||
      ${postgresql}/bin/psql $DATABASE_URL/postgres -c "CREATE DATABASE csdc"
  '';

  # Preparing the database required postgresql, which makes the image larger.
  # We can set this to true in this situation, and then set again to false, to
  # have a smaller image.
  prepareDatabase = false;

  start = pkgs.writeShellScript "start.sh" ''
    set -euo pipefail

    ${if prepareDatabase then prepareDatabaseScript else ""}

    echo "Starting..."
    /bin/csdc-server serve --config=${config}
  '';
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
      bashInteractive
    ];
    config = {
      Env = [
        "DATABASE_URL=${secrets.pgstring}"
        "GMAIL_SMTP_SERVER=${secrets.smtp_server}"
        "GMAIL_SMTP_PORT=${secrets.smtp_port}"
        "GMAIL_SMTP_LOGIN=${secrets.smtp_login}"
        "GMAIL_SMTP_PASSWORD=${secrets.smtp_password}"
      ];
      Cmd = [start];
      ExposedPorts = {
        "8080" = {};
      };
    };
    runAsRoot = ''
      mkdir -p /etc/ssl/certs
      ln -s ${cacert}/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt
      chmod 777 /bin/bash
      rm /bin/sh
      ln -s /bin/bash /bin/sh
    '';
  }
