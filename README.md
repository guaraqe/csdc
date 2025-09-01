# CSDC Network

![build](https://github.com/guaraqe/csdc/workflows/build/badge.svg)

A decentralized social network for researchers to self-organize and collaborate through democratic governance structures.

## Overview

CSDC (Computer Science Democratic Collective) is a social network platform designed to enable researchers to form self-organized structures through democratic processes. The platform combines social networking with democratic governance tools, allowing people to form units (organizations) and create hierarchical relationships through voting mechanisms.

## Key Features

### 👥 **People & Profiles**
- User registration and authentication
- Personal profiles with name, description, and image
- Profile management and updates

### 🏛️ **Units (Organizations)**
- Create and manage organizational units
- Each unit has a chairperson and membership system
- Units can form hierarchical relationships (parent/child units)
- Democratic governance with voting systems

### 🗳️ **Democratic Elections**
- Two voting systems:
  - **Simple Majority**: Traditional majority vote
  - **Majority Consensus**: Advanced majority judgment with grades (Excellent, Very Good, Good, Acceptable, Bad, Very Bad)
- Visible or secret voting options
- Automated election result computation

### 💬 **Communication System**
- **Forum**: Discussion threads and posts within units
- **Invitations**: Invite people to join units
- **Submissions**: Request to create unit relationships
- Message system with Accept/Reject replies
- Email notifications and templates

### 📁 **File Management**
- File storage and sharing within units
- IPFS integration for decentralized file storage
- Image upload and management

### 🔐 **Security & Authentication**
- JWT-based authentication
- Password hashing with bcrypt
- Session management

## Architecture

The project consists of three main components:

### Backend (Haskell)
- **csdc-api**: REST API server with Servant framework
- **csdc-base**: Core types and shared utilities
- PostgreSQL database with migrations
- IPFS integration for decentralized file storage

### Frontend (Elm)
- **csdc-gui**: Single-page application built with Elm
- Responsive web interface for all platform features
- Real-time updates and interactive forms

### Infrastructure
- Docker Compose for local development (PostgreSQL + IPFS)
- Nix for dependency management
- Fly.io deployment configuration

## Development Setup

### Prerequisites
Install [Nix](https://nixos.org/download.html) for dependency management:

```bash
curl -L https://nixos.org/nix/install | sh
```

### Quick Start

1. **Enter development environment:**
   ```bash
   nix-shell
   ```

2. **Start infrastructure services:**
   ```bash
   make docker
   ```

3. **Build the frontend:**
   ```bash
   make gui-build
   ```

4. **Start the server:**
   ```bash
   make serve
   ```

The application will be available at `http://localhost:8080`.

### Development Commands

View all available commands:
```bash
make help
```

Key development targets:
- `make ghcid-api` - Live Haskell development for API
- `make ghcid-server` - Live Haskell development for server
- `make gui-build` - Build Elm frontend
- `make psql` - Connect to development database
- `make format-haskell` - Format Haskell code

## Running the server

The first time, deploy the database and ipfs service with:

```
make docker
```

First, make sure the GUI is built with:

```
make gui-build
```

Finally, run:

```
make serve
```

and the server should be available at `localhost:8080`.

# Setting up Fly.io

Make sure to create an account with access to the free tier.

The following commands should be executed inside the Nix shell, by running
`nix-shell` before starting.

First, to login, run:

```console
$ flyctl auth login
```

Then, the Postgres database must be created:

```console
$ flyctl postgres create
```

This command will show the credentials, which should be saved in a `secrets.json` file as:

```json
{
  "pgstring": "postgresql://user:password@host:port"
}
```

Finally, create your app:

```console
$ flyctl launch
```

In this step you will choose the app name, which will determine its URL. This name should be used for the `tag-and-push-image`

In the following, the app name will be called `$APP_NAME`.

Make dure the image is in the `fly.toml` configuration file:

```toml
[build]
  image = "registry.fly.io/$APP_NAME:latest"
```

Save this name in the `secrets.json` file:

```json
{
  "app_name": "$APP_NAME",
  "pgstring": "postgresql://user:password@host:port"
}
```


# Building and deploying

Once more, all commands should be run inside the Nix shell, by running
`nix-shell` before starting.

First, build the docker image and load it:

```console
$ just build-and-load-image
```

Tag the image conveniently, and push it:

```console
$ just tag-and-push-image $APP_NAME
```

And finally, deploy:

```console
$ just deploy $APP_NAME
```

