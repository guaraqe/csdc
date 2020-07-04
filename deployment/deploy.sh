#!/usr/bin/env sh

nix-build
docker load < result
docker tag csdc-dao registry.heroku.com/csdc-dao-test/web
docker push registry.heroku.com/csdc-dao-test/web
heroku container:release -a csdc-dao-test web
