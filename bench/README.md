# Irmin Benchmarks

## Scope

The benchmark suite measures performance across several Irmin components:

| Benchmark | Description |
|-----------|-------------|
| **tree** | Tree operations (chains, large trees) using irmin-pack |
| **bench-pack** | Commit and tree performance with irmin-pack backend |
| **irmin-mem** | Commit and tree performance with in-memory backend |
| **multicore** | Parallel tree operations (half-diamond and full-diamond patterns) |
| **hashset** | Memory usage comparison of fixed-size string set implementations |

## Running Benchmarks

```bash
# Fast benchmarks (reduced parameters for quick validation)
dune build @bench-fast

# Full benchmarks (production parameters, longer runtime)
dune build @bench-full
```

Use `--force` to re-run benchmarks that have already completed:
```bash
dune build @bench-fast --force
```

## Results

All benchmark results are stored in the `_metrics/` directory at the project root. This directory persists across `dune clean` and is excluded from git.

```
_metrics/
├── bench-pack-fast/    # irmin-pack benchmark
│   ├── metrics/        # gnuplot files, PNG graphs, data
│   └── store/          # temporary Irmin store
├── irmin-mem-fast/     # in-memory backend benchmark
│   └── metrics/
├── tree-fast/          # tree operations benchmark
│   └── results.txt
├── multicore-half-fast/
│   └── metrics/half_diamond.csv
├── multicore-full-fast/
│   └── metrics/full_diamond.csv
└── hashset-fast/
    └── metrics/hashset-memory-usage.csv
```

Each benchmark prints its results location on completion:
```
Results: /path/to/irmin/_metrics/bench-pack-fast
```
