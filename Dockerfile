# syntax=docker/dockerfile:1.7-labs
# ^ we want the --parent flag for COPY

FROM ubuntu:noble as build

SHELL ["/bin/bash", "-o", "pipefail", "-c"]


ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Europe/Stockholm \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    USER_NAME=matti \
    UID=1001 \
    GID=1001

# Install dependencies
# We ignore the "pin versions" warning here, as we are not aiming for long-term
# stability.
# hadolint ignore=DL3008
RUN --mount=type=cache,id=apt-cache,target=/var/cache/apt \
    --mount=type=cache,id=apt-libs,target=/var/lib/apt \
    ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && \
    echo $TZ > /etc/timezone && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
      sudo \
      git \
      curl \
      ca-certificates \
      locales \
      build-essential \
      unzip \
      gosu \
      libffi-dev \
      libgmp-dev \
      jq \
      zstd \
      nodejs \
      npm \
      && \
    apt-get autoremove -y && \
    apt-get clean -y && \
    sed -i 's/^# *en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen && locale-gen && \
    rm -rf /var/lib/apt/lists/*

# user
RUN groupadd -g "$GID" -o "$USER_NAME" && \
    useradd -l -m -u "$UID" -g "$GID" -G sudo -o -s /bin/bash -d /home/$USER_NAME "$USER_NAME" && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# Switch to the new user
USER ${UID}:${GID}
WORKDIR /home/$USER_NAME

# toolchain env
ENV GHCUP_INSTALL_BASE_PREFIX=/home/$USER_NAME \
    HOME=/home/$USER_NAME \
    PATH=/home/$USER_NAME/.cabal/bin:/home/$USER_NAME/.ghcup/bin:$PATH \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_NO_UPGRADE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 \
    BOOTSTRAP_HASKELL_INSTALL=0
# ghcup + toolchain
RUN curl -fsSL https://get-ghcup.haskell.org -o /tmp/get-ghcup.sh && \
    chmod +x /tmp/get-ghcup.sh && \
    /tmp/get-ghcup.sh && \
    curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/-/raw/master/bootstrap.sh -o /tmp/wasm-bootstrap.sh && \
    chmod +x /tmp/wasm-bootstrap.sh && \
    /tmp/wasm-bootstrap.sh && \
    ghcup install cabal 3.14


# We copy only the cabal files, since these won't change usually. This lets us avoid
# rebuilding the dependencies all the time.
COPY --parents --chown=${UID}:${GID} *.cabal /app/
COPY --chown=${UID}:${GID} cabal.project /app/cabal.project

WORKDIR /app
# Using the cabal files, we can build the dependencies
RUN echo "packages: *.cabal" > /app/cabal.project.local &&\
    # We enable the bundled zlib, as it is more likely to work across different systems
    source /home/$USER_NAME/.ghc-wasm/env && \
    cabal --with-compiler=/home/$USER_NAME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc \
          --with-hc-pkg=/home/$USER_NAME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc-pkg \
          --with-hsc2hs=/home/$USER_NAME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-hsc2hs \
          --with-haddock=/home/$USER_NAME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-haddock \
          build --only-dependencies --project-file=cabal.project --project-dir=/app

# Copy the rest of the source code (including dpella-ffi)
COPY --parents --chown=${UID}:${GID}  src/ /app/
COPY --parents --chown=${UID}:${GID}  app/ /app/

WORKDIR /app


# Build all the packages
RUN source /home/$USER_NAME/.ghc-wasm/env && \
    cabal --with-compiler=/home/$USER_NAME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc \
          --with-hc-pkg=/home/$USER_NAME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-ghc-pkg \
          --with-hsc2hs=/home/$USER_NAME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-hsc2hs \
          --with-haddock=/home/$USER_NAME/.ghc-wasm/wasm32-wasi-ghc/bin/wasm32-wasi-haddock \
          build all --project-file=cabal.project --project-dir=/app

# Generate ghc_wasm_jsffi.js from the compiled wasm file
RUN source /home/$USER_NAME/.ghc-wasm/env && \
    WASM_FILE=$(find dist-newstyle -name "test.wasm" -type f | head -n 1) && \
    if [ -n "$WASM_FILE" ]; then \
        LIBDIR=$(wasm32-wasi-ghc --print-libdir) && \
        node "$LIBDIR/post-link.mjs" -i "$WASM_FILE" -o /app/ghc_wasm_jsffi.js; \
    else \
        echo "Warning: test.wasm not found, skipping JSFFI generation"; \
    fi

# Copy the scripts, datasets, tests, and extra files
COPY --chown=${UID}:${GID} LICENSE README.md /app/

USER root
# Default command
CMD ["/bin/bash"]
