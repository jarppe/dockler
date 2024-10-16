# Dockler - Pure Clojure API for Docker daemon API

Clojure API for [Docker HTTP API version 1.46](https://docs.docker.com/engine/api/v1.46/).

This library tries to be compatible with [Babashka](https://babashka.org/). Currently there are
couple of places that need a workarounds.

The only dependency it has is [cheshire](https://github.com/dakrone/cheshire), which is already included in Babashka.

Tested with Clojure 1.11.4 and Java 23.

## Status

This is an alpha library intended for education and experimentation only.

## TODO

- building images
- docker-compose like functionality
- documentation
- babashka support
