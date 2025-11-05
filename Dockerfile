# syntax=docker/dockerfile:1.7-labs
# ^ we want the --parent flag for COPY

FROM ubuntu:noble as build-dpella

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Args
ARG USER_NAME=dpella
ARG GHC_VERSION=9.6.6
ARG CABAL_VERSION=3.16.0.0
ARG UID=1001
ARG GID=1001

ENV DEBIAN_FRONTEND=noninteractive \
    TZ=Europe/Stockholm \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    USER_NAME=${USER_NAME}\
    UID=${UID} \
    GID=${GID}


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
      libncurses-dev \
      libsqlite3-dev \
      zlib1g-dev \
      libtinfo-dev \
      postgresql \
      postgresql-contrib \
      postgresql-server-dev-all \
      libpq-dev \
      z3 \
      # pyspark
      python3-venv \
      python3-pip \
      openjdk-17-jre-headless \
      libssl-dev \
      libisal-dev \
      libpmem-dev \
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
    ghcup install ghc "$GHC_VERSION" && \
    ghcup set ghc "$GHC_VERSION" && \
    ghcup install cabal "$CABAL_VERSION" && \
    cabal --version && ghc --version && \
    rm -f /tmp/get-ghcup.sh && \
    rm -rf /home/$USER_NAME/.ghcup/cache/* /home/$USER_NAME/.ghcup/logs/* /home/$USER_NAME/.ghcup/tmp/*

USER root
# We add /opt/hadoop to the PATH, so that pyspark can find it
ADD https://downloads.apache.org/hadoop/common/hadoop-3.3.6/hadoop-3.3.6.tar.gz /opt/
ENV JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
RUN tar -xzf /opt/hadoop-3.3.6.tar.gz -C /opt/ && \
    rm /opt/hadoop-3.3.6.tar.gz && \
    ln -s /opt/hadoop-3.3.6 /opt/hadoop; \
    /opt/hadoop/bin/hadoop classpath > /etc/hadoop.classpath

# Hadoop env for native + classpath
# Note: compute SPARK_DIST_CLASSPATH at build time so PySpark finds Hadoop
ENV HADOOP_HOME=/opt/hadoop \
    HADOOP_COMMON_LIB_NATIVE_DIR=/opt/hadoop/lib/native \
    LD_LIBRARY_PATH=/opt/hadoop/lib/native:${LD_LIBRARY_PATH} \
    PATH=/opt/hadoop/bin:${PATH}

# Sanity check: fail the build if native isn’t visible
RUN SPARK_DIST_CLASSPATH=$(cat /etc/hadoop.classpath) && \
    export SPARK_DIST_CLASSPATH && \
    ${HADOOP_HOME}/bin/hadoop checknative -a


# Install libduckdb

WORKDIR /tmp
RUN  curl -L -o /tmp/libduckdb.zip https://github.com/duckdb/duckdb/releases/download/v1.4.1/libduckdb-linux-amd64.zip && \
    unzip libduckdb.zip && \
    mv libduckdb.so /usr/lib/libduckdb.so && \
    mv duckdb.h /usr/include/ && \
    ldconfig && \
    rm libduckdb.zip

USER ${UID}:${GID}

# Copy pydpella dependencies
COPY --chown=${UID}:${GID} pydpella/global-requirements.txt /app/pydpella/global-requirements.txt

# Install global Python requirements, including pyspark and pydpella path
RUN --mount=type=cache,id=pip-cache,target=/root/.cache/pip \
    python3 -m venv /app/venv; \
    /app/venv/bin/python -m pip install --no-cache-dir pip==25.2 setuptools==80.9.0 wheel==0.45.1; \
    /app/venv/bin/python -m pip install --no-cache-dir -r /app/pydpella/global-requirements.txt;

# We copy only the cabal files, since these won't change usually. This lets us avoid
# rebuilding the dependencies all the time.
COPY --parents --chown=${UID}:${GID} --exclude=dpella-ffi/*.cabal  dpella-*/*.cabal /app/
COPY --chown=${UID}:${GID} cabal.project /app/cabal.project
COPY --chown=${UID}:${GID} cabal.project.freeze /app/cabal.project.freeze

WORKDIR /app
# Using the cabal files, we can build the dependencies
RUN echo "packages: **/dpella-*.cabal" > /app/cabal.project.local &&\
    # We enable the bundled zlib, as it is more likely to work across different systems
    sed -i 's/-bundled-c-zlib/+bundled-c-zlib/' cabal.project.freeze && \
    cabal update --index-state='2025-10-20T13:41:13Z' && \
    cabal build all --only-dependencies --project-file=cabal.project --project-dir=/app

# Copy the rest of the source code (including dpella-ffi)
COPY --link --parents --chown=${UID}:${GID}  dpella-*/ /app/

WORKDIR /app

# Initialize a git repository, so that a version can be inferred
# We set a fixed date, so that the builds are reproducible
RUN git config --global user.name ${USER_NAME}; \
    git config --global user.email "${USER_NAME}@dpella.io"; \
    git init .; \
    GIT_AUTHOR_DATE="2025-10-20T13:41:13Z"; \
    export GIT_AUTHOR_DATE; \
    GIT_COMMITTER_DATE="$GIT_AUTHOR_DATE"; \
    export GIT_COMMITTER_DATE; \
    git commit --allow-empty -m "Init"


# Build all the packages
RUN cabal build all --project-file=cabal.project --project-dir=/app

# Copy the scripts, datasets, tests, and extra files
COPY --chown=${UID}:${GID} LICENSE README.md /app/
COPY --chown=${UID}:${GID} datasets /app/datasets
COPY --chown=${UID}:${GID} --chmod=+x scripts /app/scripts

# Copy rest of pydpella
COPY --chown=${UID}:${GID} pydpella /app/pydpella


# Build and install the ffi package, this sets the ldconfig as well.
# Also add dpella-compile to the bin directory, for the python library
USER root
RUN /app/scripts/debian-ffi-build 123 && \
    dpkg -i dpella-ffi_123.deb && \
    rm dpella-ffi_123.deb && \
    dpella_compile_bin=$(cabal list-bin dpella-compile | tail -n1) && \
    install -D -m 0755 "$dpella_compile_bin" /usr/local/bin/dpella-compile && \
    dpella_server_bin=$(cabal list-bin dpella-server | tail -n1) && \
    install -D -m 0755 "$dpella_server_bin" /usr/local/bin/dpella-server && \
    dpella_lsp_bin=$(cabal list-bin dpella-lsp | tail -n1) && \
    install -D -m 0755 "$dpella_lsp_bin" /usr/local/bin/dpella-lsp && \
    dpella_repl_bin=$(cabal list-bin dpella-repl:exe:dpella-repl | tail -n1) && \
    install -D -m 0755 "$dpella_repl_bin" /usr/local/bin/dpella-repl

COPY --chown=${UID}:${GID} --chmod=+x tests/ /app/tests
# Install pydpella itself
RUN /app/venv/bin/python -m pip install --no-cache-dir -e /usr/local/lib/dpella/pydpella

# Initialize the database with the init script
COPY ./scripts/init-postgres.sql /docker-entrypoint-initdb.d/init-postgres.sql
RUN ldconfig && \
    pg_ctlcluster --skip-systemctl-redirect 16 main start && \
    gosu postgres psql --user=postgres --file=/docker-entrypoint-initdb.d/init-postgres.sql

COPY --chmod=0755 <<EOT /usr/local/bin/entrypoint
#!/usr/bin/env bash
set -euo pipefail
cd /app
exec "\$@"
EOT

# We need to be root to start the postgres server
# hadolint ignore=DL3002
USER root
WORKDIR /app

# Default command
ENTRYPOINT ["/usr/local/bin/entrypoint"]
CMD ["/bin/bash"]
