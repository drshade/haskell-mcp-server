# Build stage
FROM haskell:9.10 AS builder

RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get update --allow-releaseinfo-change && \
    apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN cabal update

# Copy and build
WORKDIR /app
COPY mcp-server.cabal /app
COPY README.md /app
COPY CHANGELOG.md /app
COPY LICENSE /app

RUN cabal build all --only-dependencies

COPY src /app/src
COPY examples /app/examples
COPY test /app/test

RUN cabal build all

# Install all executables to /usr/local/bin
RUN cabal install exe:http-simple-example --installdir=/usr/local/bin
RUN cabal install exe:complete-example --installdir=/usr/local/bin
RUN cabal install exe:simple-example --installdir=/usr/local/bin

# Runtime stage
FROM debian:bullseye-slim

RUN apt-get update && \
    apt-get install -y \
    ca-certificates \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

# Copy all binaries from builder
COPY --from=builder /usr/local/bin/http-simple-example /usr/local/bin/http-simple-example
COPY --from=builder /usr/local/bin/complete-example /usr/local/bin/complete-example
COPY --from=builder /usr/local/bin/simple-example /usr/local/bin/simple-example

WORKDIR /app

# Default entrypoint is the main executable
ENTRYPOINT ["/usr/local/bin/haskell-mcp-lib"]