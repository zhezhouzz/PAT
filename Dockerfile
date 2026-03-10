FROM ubuntu:22.04
ENV DEBIAN_FRONTEND=noninteractive

# System dependencies
RUN apt-get update && apt-get install -y \
    build-essential gcc g++ make git curl wget \
    python3 python3-pip libgmp-dev pkg-config \
    bubblewrap unzip rsync apt-transport-https \
    ca-certificates gnupg \
  && rm -rf /var/lib/apt/lists/*

# Install .NET SDK 8.0 (required by the P language toolchain)
RUN curl -fsSL https://dot.net/v1/dotnet-install.sh \
    | bash -s -- --channel 8.0 --install-dir /usr/local/dotnet
ENV PATH="/usr/local/dotnet:${PATH}"
ENV DOTNET_ROOT="/usr/local/dotnet"

# Install the P language tool
RUN dotnet tool install --global P
ENV PATH="/root/.dotnet/tools:${PATH}"

# Install opam
RUN curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh \
    | bash -s -- --no-backup
RUN opam init --disable-sandboxing --auto-setup -y

# Create OCaml 5.2.0 switch
RUN opam switch create 5.2.0 ocaml-base-compiler.5.2.0 -y

# Install OCaml dependencies
RUN eval $(opam env) && opam install -y \
    dune core core_unix yojson conf-c++ conf-python qcheck \
    ocolor dolog ocamlbuild z3 ppx_deriving_yojson \
    menhirLib menhir spectrum ppx_jane

# Install zutils
RUN eval $(opam env) && \
    git clone https://github.com/zhezhouzz/zutils.git /tmp/zutils && \
    cd /tmp/zutils && opam install . -y

# Install AutomataLibrary
RUN eval $(opam env) && \
    git clone https://github.com/zhezhouzz/AutomataLibrary.git /tmp/AutomataLibrary && \
    cd /tmp/AutomataLibrary && opam install . -y

# Copy repository and build Clouseau
COPY . /home/clouseau
WORKDIR /home/clouseau
RUN mkdir -p stat output
RUN eval $(opam env) && \
    dune build --profile release && \
    cp _build/default/bin/main.exe main.exe

# Source opam environment on every shell invocation
ENTRYPOINT ["/bin/bash", "-c", "eval $(opam env) && exec \"$@\"", "--"]
CMD ["bash"]
