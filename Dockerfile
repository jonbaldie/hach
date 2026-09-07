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

# Create directory stubs referenced by cabal, build, and strip executable
RUN mkdir -p test test-integration && \
    cabal build exe:hach && \
    mkdir -p /build/bin && \
    cp "$(cabal list-bin exe:hach)" /build/bin/hach && \
    strip /build/bin/hach

# ------------------------------------------------------------------------------
# Runtime Stage
# ------------------------------------------------------------------------------
FROM debian:bookworm-slim AS runtime

# Install runtime dependencies and system tools
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        gh \
        git \
        libffi8 \
        libgmp10 \
        libnotify-bin \
        netbase \
        tini \
        zlib1g && \
    rm -rf /var/lib/apt/lists/*

# UTF-8 locale and terminal settings for Haskell & Brick TUI
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TERM=xterm-256color

# Configure git safe.directory and set up non-root user and workspace
RUN git config --system --add safe.directory '*' && \
    useradd -m -u 1000 -U -s /bin/bash hach && \
    mkdir -p /workspace && \
    chown -R hach:hach /workspace

# Copy compiled binary from builder
COPY --from=builder /build/bin/hach /usr/local/bin/hach

USER 1000:1000
WORKDIR /workspace

ENTRYPOINT ["/usr/bin/tini", "--", "hach"]
CMD []
