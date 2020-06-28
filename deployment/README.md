# Deployment

## Creating the Docker image

To create the Docker image, do:

```
nix-build
docker load < result
```

To run the image and have the server accessible at `localhost:8080`, run:

```
export SECRET_TOKEN=...
export SECRET_ORCID_ID=...
export SECRET_ORCID_SECRET=...
docker run --rm \
  -e SECRET_TOKEN -e SECRET_ORCID_ID -e SECRET_ORCID_SECRET \
  -p 127.0.0.1:8080:8080/tcp \
  csdc-dao
```

To inspect the Docker image, do:

```
docker run -it csdc-dao sh
```
