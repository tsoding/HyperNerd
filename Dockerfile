################################################################################
# Builder                                                                      #
################################################################################
FROM alpine:3.9 as builder

MAINTAINER Alexey Kutepov <reximkut@gmail.com>

# Have base OS package installation in a single layer
RUN \
  apk update && \
  apk add --no-cache \
    openssl-dev curl ghc cabal \
    gmp-dev build-base zlib-dev && \
  cabal update && \
  mkdir -p /build/

WORKDIR /build/
COPY . /build/

# Install necessary dependencies and build HyperNerd
# NOTE: happy package provides an executable that is needed to build
# qm-interpolated-string package. qm-interpolated-string does not
# depend on happy for some reason and that's why we have to install it
# separately.
RUN \
  cabal install happy-1.19.9 && \
  cabal install -j4 --only-dependencies && \
  cabal build exe:HyperNerd


################################################################################
# Application                                                                  #
################################################################################
FROM alpine:3.9 AS app

RUN \
  apk update && \
  apk add --no-cache \
    gmp libffi zlib openssl curl && \
  mkdir -p /app

COPY --from=builder /build/dist/build/HyperNerd/HyperNerd /app
WORKDIR /app

CMD ["/app/HyperNerd", "/tmp/hypernerd/secret.ini", "/tmp/hypernerd/database.db",  "/tmp/hypernerd/markov.csv"]
