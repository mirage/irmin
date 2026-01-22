# Inline Contents Benchmark

This benchmark measures the effectiveness of inlining small contents directly
in nodes, comparing read latency and storage size across different content
size distributions.

## Building

```bash
dune build test/irmin-pack/bench_inline/main.exe
```

## Running Benchmarks

### Single Distribution

Run a single distribution with specific parameters:

```bash
dune exec test/irmin-pack/bench_inline/main.exe -- run \
  -d zipfian-small \
  --dist-name zipfian-small \
  --inline \
  -t 48 \
  -n 1000 \
  --reads 200 \
  --runs 3 \
  --header
```

Options:
- `-d, --distribution`: Size distribution (see below)
- `--dist-name`: Name for CSV output
- `--inline`: Enable inlining
- `-t, --threshold`: Inlining threshold in bytes (default: 48)
- `-n, --contents`: Number of contents to create (default: 10000)
- `--reads`: Number of read operations for latency measurement (default: 1000)
- `--runs`: Number of benchmark iterations (default: 5)
- `--header`: Print CSV header

### All Distributions

Run all distributions with and without inlining:

```bash
dune exec test/irmin-pack/bench_inline/main.exe -- run --all \
  -n 1000 \
  --reads 200 \
  --runs 3
```

### Threshold Optimization

Find optimal threshold by testing multiple values (8, 16, 24, 32, 48, 64, 96, 128 bytes):

```bash
dune exec test/irmin-pack/bench_inline/main.exe -- optimize \
  -n 1000 \
  --reads 200 \
  --runs 3
```

### Threshold Sweep

Test content sizes around a specific threshold:

```bash
dune exec test/irmin-pack/bench_inline/main.exe -- sweep \
  -t 48 \
  -r 10 \
  -n 500 \
  --reads 100 \
  --runs 2
```

## Available Distributions

| Name | Description |
|------|-------------|
| `uniform-small` | Uniform 1-10 bytes (all inlined) |
| `uniform-large` | Uniform 50-200 bytes (none inlined) |
| `around-threshold` | Uniform 5-30 bytes (mix around threshold) |
| `mostly-small` | Bimodal: 80% 8-byte, 20% 100-byte |
| `mostly-large` | Bimodal: 20% 8-byte, 80% 100-byte |
| `log-normal-small` | Log-normal, median ~20 bytes |
| `log-normal-medium` | Log-normal, median ~150 bytes |
| `zipfian-small` | Zipfian (s=1.0), 1-100 bytes |
| `zipfian-medium` | Zipfian (s=1.0), 1-1000 bytes |
| `zipfian-steep` | Zipfian (s=1.5), 1-100 bytes (more skewed) |

## Output Metrics

The benchmark outputs CSV with the following columns:

| Column | Description |
|--------|-------------|
| `distribution` | Distribution name |
| `threshold` | Inlining threshold ("off" or bytes) |
| `n_contents` | Number of contents |
| `store_bytes` | Total store size on disk |
| `write_ms` | Write time in milliseconds |
| `read_p50_us` | Read latency p50 in microseconds |
| `read_p99_us` | Read latency p99 in microseconds |
| `read_p999_us` | Read latency p999 in microseconds |
| `inlined` | Number of inlined contents |
| `non_inlined` | Number of non-inlined contents |
| `size_stats` | Size distribution statistics |

## Generating Plots

After running benchmarks, update the data file and generate plots:

```bash
cd test/irmin-pack/bench_inline

# Run benchmark and save results
dune exec ./main.exe -- run --all -n 1000 --reads 200 --runs 3 > results.csv

# Update data file (manually extract p99 values from results.csv)
# Format: Distribution  No-Inline  Inline-48  Improvement%

# Generate comparison plot
gnuplot bench_inline_plot.gp

# Generate improvement plot
gnuplot bench_inline_improvement.gp
```

### Plot Files

- `bench_inline_comparison.png` - Side-by-side latency comparison (no-inline vs inline)
- `bench_inline_improvement.png` - Percentage improvement chart
- `bench_inline_plot.gp` - Gnuplot script for comparison chart
- `bench_inline_improvement.gp` - Gnuplot script for improvement chart
- `bench_inline_plot.dat` - Data file for plots

## Example Results

With threshold=48 bytes, typical improvements in read p99 latency:

| Distribution | Improvement |
|--------------|-------------|
| uniform-small | 63% |
| mostly-small | 55% |
| zipfian-small | 49% |
| zipfian-steep | 49% |
| zipfian-medium | 37% |
| around-threshold | 34% |
| mostly-large | 21% |
| log-normal-small | 21% |
| log-normal-medium | 6% |
| uniform-large | -2% (no small values) |
