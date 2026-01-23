# Inline Contents Benchmark

This benchmark measures the effectiveness of inlining small contents directly
in nodes, comparing read latency and storage size across different content
size distributions.

## Quick Start

Run the complete benchmark suite and generate all plots with a single command:

```bash
make bench-inline
```

This will:
1. Build the benchmark
2. Run all distributions with and without inlining
3. Generate data files for gnuplot
4. Create three plots: latency comparison, latency improvement, and storage impact

Results are saved in `test/irmin-pack/bench_inline/`.

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

After running benchmarks, generate data files and plots:

```bash
cd test/irmin-pack/bench_inline

# Run benchmark and save results
dune exec ./main.exe -- run --all -n 5000 --reads 500 --runs 3 > results.csv

# Generate data files from CSV
bash generate_data.sh results.csv

# Generate all plots
gnuplot bench_inline_plot.gp
gnuplot bench_inline_improvement.gp
gnuplot bench_inline_storage.gp
```

Or use the Makefile target which does all of this:

```bash
make bench-inline
```

### Plot Files

- `bench_inline_comparison.png` - Side-by-side latency comparison (no-inline vs inline)
- `bench_inline_improvement.png` - Percentage improvement chart
- `bench_inline_storage.png` - Storage consumption impact chart
- `bench_inline_plot.gp` - Gnuplot script for comparison chart
- `bench_inline_improvement.gp` - Gnuplot script for improvement chart
- `bench_inline_storage.gp` - Gnuplot script for storage impact chart
- `bench_inline_plot.dat` - Data file for latency plots
- `bench_inline_storage.dat` - Data file for storage plots
- `generate_data.sh` - Script to generate data files from CSV output

## Example Results

### Read Latency Improvement

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

### Storage Impact

Storage consumption change with threshold=48 bytes:

| Distribution | Storage Change | Inlined % |
|--------------|----------------|-----------|
| uniform-small | -1.4% | 100% |
| uniform-large | +0.1% | 0% |
| around-threshold | +7.4% | 100% |
| mostly-small | +1.0% | 80% |
| mostly-large | +0.5% | 19% |
| log-normal-small | +5.1% | 79% |
| log-normal-medium | -0.2% | 13% |
| zipfian-small | +1.8% | 85% |
| zipfian-medium | +0.4% | 59% |
| zipfian-steep | -1.3% | 96% |

**Key observations:**
- Very small content (zipfian-steep, uniform-small): slight storage **reduction** (-1% to -1.4%)
- Content around threshold (around-threshold, log-normal-small): storage **increase** (+5% to +7%)
- Large content (uniform-large, log-normal-medium): negligible impact

The storage trade-off is explained by:
1. Inlining avoids separate content entries (saves hash reference overhead)
2. But inlined content loses content-addressable deduplication
3. When content is very small, entry overhead dominates → inlining saves space
4. When content is near threshold, duplication overhead dominates → inlining costs space
