# Spice Docker image for Irmin-eio
#
# Build:
#   docker build -t spice .
#
# Serve documentation:
#   docker run --rm -p 8080:8080 spice docs
#
# Run tests:
#   docker run --rm spice test
#
# Run benchmarks (results exported to host via bind mount):
#   mkdir -p _metrics && chmod 777 _metrics
#   docker run --rm -v $(pwd)/_metrics:/home/opam/irmin-eio/_metrics spice bench-fast
#   docker run --rm -v $(pwd)/_metrics:/home/opam/irmin-eio/_metrics spice bench-full
#
# Interactive shell:
#   docker run --rm -it spice shell

FROM ocaml/opam:ubuntu-ocaml-5.3

# Install system dependencies
RUN sudo apt-get update && sudo apt-get install -y \
    libffi-dev \
    gnuplot \
    libgmp-dev \
    libssl-dev \
    pkg-config \
    busybox \
    && sudo apt-get clean \
    && sudo rm -rf /var/lib/apt/lists/*

# Set up opam environment
RUN opam update

# Pin external dependencies at specific commits
RUN opam pin add -n index.dev git+https://github.com/mirage/index#6e84bed48db8ab53a09926b45d899815c30f2b39
RUN opam pin add -n irmin-watcher.dev git+https://github.com/patricoferris/irmin-watcher#d0e92b4ba5631b5f4dc0f3c00d97e79542dba45d
RUN opam pin add -n qcheck-multicoretests-util.0.10 git+https://github.com/lyrm/multicoretests#6dc52b7b51215888f57147e7954cc90736ca4f3b
RUN opam pin add -n qcheck-stm.0.10 git+https://github.com/lyrm/multicoretests#6dc52b7b51215888f57147e7954cc90736ca4f3b

# Copy source code
WORKDIR /home/opam/irmin-eio
COPY --chown=opam:opam . .

# Pin all local packages
RUN opam pin add -n irmin.dev . \
    && opam pin add -n irmin-bench.dev . \
    && opam pin add -n irmin-chunk.dev . \
    && opam pin add -n irmin-cli.dev . \
    && opam pin add -n irmin-client.dev . \
    && opam pin add -n irmin-containers.dev . \
    && opam pin add -n irmin-fs.dev . \
    && opam pin add -n irmin-git.dev . \
    && opam pin add -n irmin-graphql.dev . \
    && opam pin add -n irmin-mirage.dev . \
    && opam pin add -n irmin-mirage-git.dev . \
    && opam pin add -n irmin-mirage-graphql.dev . \
    && opam pin add -n irmin-pack.dev . \
    && opam pin add -n irmin-pack-tools.dev . \
    && opam pin add -n irmin-server.dev . \
    && opam pin add -n irmin-test.dev . \
    && opam pin add -n irmin-tezos.dev . \
    && opam pin add -n libirmin.dev . \
    && opam pin add -n ppx_irmin.dev .

# Install dependencies and build
RUN opam install . --deps-only --with-test --with-doc -y
RUN opam exec -- dune build

# Generate documentation
RUN opam exec -- dune build @doc

# Copy entrypoint script
COPY --chown=opam:opam docker-entrypoint.sh /home/opam/
RUN chmod +x /home/opam/docker-entrypoint.sh

EXPOSE 8080

ENTRYPOINT ["/home/opam/docker-entrypoint.sh"]
CMD ["help"]
