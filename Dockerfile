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
    dune core core_unix yojson conf-c++ qcheck \
    ocolor dolog ocamlbuild ppx_deriving_yojson \
    menhirLib menhir spectrum ppx_jane

# Install z3 separately (compiles from source; may be slow, ~400s)
RUN opam install -y z3

# Install zutils (pinned to commit 4b0e5cad, OCamlRefinementType org)
RUN --mount=type=ssh \
    mkdir -p -m 0700 ~/.ssh && ssh-keyscan github.com >> ~/.ssh/known_hosts && \
    git clone git@github.com:OCamlRefinementType/zutils.git /tmp/zutils && \
    cd /tmp/zutils && git checkout 4b0e5cad8c6a1591601e3f9a45a6da33fbb54657 && \
    opam install . -y

# Install AutomataLibrary (pinned to commit 48a2a231, v1.0 branch)
RUN --mount=type=ssh \
    git clone git@github.com:OCamlRefinementType/AutomataLibrary.git /tmp/AutomataLibrary && \
    cd /tmp/AutomataLibrary && git checkout 48a2a23152b4f5a6253b673d30d99ed5f69f6b28 && \
    opam install . -y

# Copy repository and build Clouseau
USER root
COPY --chown=opam:opam . /home/clouseau
WORKDIR /home/clouseau
USER opam
RUN mkdir -p stat output && \
    eval $(opam env) && \
    dune build --profile release && \
    cp _build/default/bin/main.exe main.exe

# Source opam environment on every shell invocation
ENTRYPOINT ["/bin/bash", "-c", "eval $(opam env) && exec \"$@\"", "--"]
CMD ["bash"]
