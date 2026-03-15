# syntax=docker/dockerfile:1
# Base image: Ubuntu 25.04 with OCaml 5.2 + opam pre-installed
# Avoids rebuilding the OCaml compiler from scratch
FROM ocaml/opam:ubuntu-25.04-ocaml-5.2

USER root
ENV DEBIAN_FRONTEND=noninteractive

# System dependencies
RUN apt-get update && apt-get install -y \
    build-essential gcc g++ make git curl wget \
    python3 python3-pip libgmp-dev pkg-config \
    bubblewrap unzip rsync apt-transport-https \
    ca-certificates gnupg openssh-client \
  && rm -rf /var/lib/apt/lists/*

# Install .NET SDK 8.0 (required by the P language toolchain)
RUN curl -fsSL https://dot.net/v1/dotnet-install.sh \
    | bash -s -- --channel 8.0 --install-dir /usr/local/dotnet
ENV PATH="/usr/local/dotnet:${PATH}"
ENV DOTNET_ROOT="/usr/local/dotnet"
ENV DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1

# Install the P language tool
RUN dotnet tool install --global P
ENV PATH="/root/.dotnet/tools:${PATH}"

# Switch to opam user for all OCaml/opam operations
USER opam
WORKDIR /home/opam

# Install OCaml dependencies
# Note: conf-python was removed (no longer in opam; python3 already installed via apt)
RUN opam install -y \
    dune.3.21.1 core.v0.17.1 core_unix.v0.17.1 yojson.3.0.0 conf-c++.1.0 qcheck.0.91 \
    ocolor.1.3.1 dolog.6.0.0 ocamlbuild.0.16.1 ppx_deriving_yojson.3.9.1 \
    menhirLib.20260209 menhir.20260209 spectrum.1.0.0.alpha ppx_jane.v0.17.0

# Install z3 separately (compiles from source; may be slow, ~400s)
RUN opam install -y z3.4.15.2

# Install MariaDB system library (needed by the mariadb opam package)
USER root
RUN apt-get update && apt-get install -y libmariadb-dev && rm -rf /var/lib/apt/lists/*
USER opam

# Install additional OCaml dependencies
RUN opam install -y lwt.5.9.2 lwt_ppx.5.9.1 qcheck-stm.0.10 mariadb.1.3.0

# Install zutils (pinned to commit 4b0e5cad, OCamlRefinementType org)
RUN git clone https://github.com/OCamlRefinementType/zutils.git /tmp/zutils && \
    cd /tmp/zutils && git checkout 4b0e5cad8c6a1591601e3f9a45a6da33fbb54657 && \
    opam install . -y

# Install AutomataLibrary (tag v2.1)
RUN git clone --branch v2.1 https://github.com/OCamlRefinementType/AutomataLibrary.git /tmp/AutomataLibrary && \
    opam install /tmp/AutomataLibrary -y

# Clone Clouseau (artifact branch) and build
# Pass --build-arg CACHE_BUST=$(date +%s) to force re-clone
ARG CACHE_BUST=1
USER root
RUN mkdir -p /home/clouseau && chown opam:opam /home/clouseau
USER opam
RUN git clone --branch artifact https://github.com/zhezhouzz/PAT.git /home/clouseau
WORKDIR /home/clouseau
RUN mkdir -p stat output && \
    eval $(opam env) && \
    dune build --profile release && \
    cp _build/default/bin/main.exe main.exe

# Source opam environment on every shell invocation
ENTRYPOINT ["/bin/bash", "-c", "eval $(opam env) && exec \"$@\"", "--"]
CMD ["bash"]
