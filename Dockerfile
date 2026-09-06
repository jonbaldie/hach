# syntax=docker/dockerfile:1

# ------------------------------------------------------------------------------
# Build Stage
# ------------------------------------------------------------------------------
FROM haskell:9.12-bookworm AS builder

WORKDIR /build

# Pre-cache Cabal dependencies
COPY hach.cabal ./
RUN cabal update && \
    cabal build --only-dependencies lib:hach

# Copy application source code
COPY app/ app/
COPY src/ src/
COPY LICENSE README.md ./

# Build the executable
RUN cabal build exe:hach

# Install binary to /build/bin and strip symbols
RUN mkdir -p /build/bin && \
    cp "$(cabal list-bin exe:hach)" /build/bin/hach && \
    strip /build/bin/hach

# ------------------------------------------------------------------------------
# Runtime Stage
# ------------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        libgmp10 \
        libffi8 \
        zlib1g \
        git \
        curl \
        netbase && \
    rm -rf /var/lib/apt/lists/*

# UTF-8 locale and terminal settings for Haskell & Brick TUI
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TERM=xterm-256color

# Create non-root user and workspace directory
RUN useradd -m -u 1000 -s /bin/bash hach && \
    mkdir -p /workspace && \
    chown -R hach:hach /workspace

# Copy compiled binary from builder
COPY --from=builder /build/bin/hach /usr/local/bin/hach

USER hach
WORKDIR /workspace

ENTRYPOINT ["hach"]
CMD []
