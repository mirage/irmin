#!/bin/bash
set -e

cd /home/opam/irmin-eio

case "$1" in
    docs|serve-docs)
        echo "Serving documentation on http://0.0.0.0:8080"
        echo "Open http://localhost:8080 in your browser"
        cd _build/default/_doc/_html
        busybox httpd -f -p 8080
        ;;
    test|tests)
        echo "Running tests..."
        shift
        opam exec -- dune runtest "$@"
        ;;
    bench-fast)
        echo "Running fast benchmark suite..."
        opam exec -- make bench-fast
        ;;
    bench-full)
        echo "Running full benchmark suite..."
        opam exec -- make bench-full
        ;;
    shell|bash)
        echo "Starting shell..."
        exec /bin/bash
        ;;
    build)
        echo "Building..."
        shift
        opam exec -- dune build "$@"
        ;;
    help|--help|-h|"")
        echo "Spice - Irmin-eio Docker image"
        echo ""
        echo "Usage: docker run [options] trustchainspice/spice <command>"
        echo ""
        echo "Commands:"
        echo "  help        Show this help message (default)"
        echo "  docs        Serve API documentation on port 8080"
        echo "  test        Run the test suite"
        echo "  bench-fast  Run fast benchmark suite"
        echo "  bench-full  Run full benchmark suite"
        echo "  build       Build the project"
        echo "  shell       Start an interactive shell"
        echo ""
        echo "Examples:"
        echo "  docker run --rm trustchainspice/spice help"
        echo "  docker run --rm -p 8080:8080 trustchainspice/spice docs"
        echo "  docker run --rm trustchainspice/spice test"
        echo "  docker run --rm -it trustchainspice/spice shell"
        echo ""
        echo "Benchmarks (use bind mount to export results to host):"
        echo "  mkdir -p _metrics && chmod 777 _metrics"
        echo "  docker run --rm -v \$(pwd)/_metrics:/home/opam/irmin-eio/_metrics trustchainspice/spice bench-fast"
        echo "  docker run --rm -v \$(pwd)/_metrics:/home/opam/irmin-eio/_metrics trustchainspice/spice bench-full"
        ;;
    *)
        echo "Unknown command: $1"
        echo "Run 'docker run --rm trustchainspice/spice help' for usage information."
        exit 1
        ;;
esac
