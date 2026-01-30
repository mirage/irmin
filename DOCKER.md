# Spice Docker Image

Pre-built Docker image for Irmin-eio with benchmarks and documentation.

**Image:** [trustchainspice/spice](https://hub.docker.com/r/trustchainspice/spice)

## Quick Start

```bash
# Interactive shell
docker run --rm -it trustchainspice/spice shell

# Serve API documentation on http://localhost:8080
docker run --rm -p 8080:8080 trustchainspice/spice docs

# Run tests
docker run --rm trustchainspice/spice test

# Show help
docker run --rm trustchainspice/spice help
```

## Benchmarks

Results are exported to the host via bind mount:

```bash
mkdir -p _metrics && chmod 777 _metrics

# Fast benchmark suite
docker run --rm -v $(pwd)/_metrics:/home/opam/irmin-eio/_metrics trustchainspice/spice bench-fast

# Full benchmark suite
docker run --rm -v $(pwd)/_metrics:/home/opam/irmin-eio/_metrics trustchainspice/spice bench-full
```

Results will be in `_metrics/<uuid>/` including CSV data, JSON reports, and PNG plots.

## Building Locally

```bash
docker build -t spice .
```
