# syntax=docker/dockerfile:1

FROM haskell:9.12-bookworm

# Install development tools and system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        curl \
        git \
        libffi-dev \
        libgmp-dev \
        procps \
        ripgrep \
        zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

# UTF-8 locale and terminal settings for Haskell & Brick TUI
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TERM=xterm-256color

# Pre-cache Cabal dependencies for external libraries
WORKDIR /tmp/cabal-cache
COPY hach.cabal ./
RUN cabal update && \
    cabal build --only-dependencies lib:hach && \
    rm -rf /tmp/cabal-cache

# Working directory for development
WORKDIR /workspace

# Copy source repository
COPY . /workspace

# Pre-compile the project and test suite
RUN cabal build --enable-tests all

CMD ["/bin/bash"]
