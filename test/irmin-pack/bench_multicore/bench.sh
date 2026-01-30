#!/bin/sh

elements=1000
finds=""
runs=10
domains_arg=""

usage() {
  echo "Usage: $0 [-e elements] [-f finds] [-r runs] [-d domains]"
  exit 1
}

while getopts "e:f:r:d:h" o; do
    case "${o}" in
        e)
            elements=${OPTARG}
            ;;
        f)
            finds=${OPTARG}
            ;;
        r)
            runs=${OPTARG}
            ;;
        d)
            domains_arg=${OPTARG}
            ;;
        h)
            usage
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))

if [ -z "$finds" ]; then
  finds="$elements"
fi

if [ -z "$domains_arg" ]; then
  cores=$(nproc)
  max_domains=$((cores - 1))
  if [ "$max_domains" -lt 1 ]; then max_domains=1; fi
  seq_args="1 $max_domains"
else
  # Replace - with space to support min-max format
  seq_args=$(echo "$domains_arg" | tr '-' ' ')
  # If only one number provided, assume 1 to N
  if ! echo "$seq_args" | grep -q " "; then
    seq_args="1 $seq_args"
  fi
fi

# Get absolute path to project root (more robust than relative path)
project_root="$(cd ../../.. && pwd)"
csv_file="$project_root/_build/bench-multicore/metrics/half_diamond.csv"

echo "# domains,min_time,median_time,max_time,min_ratio,median_ratio,max_ratio,joules,milliseconds"

for i in $(seq $seq_args); do
  # Use LC_ALL=C to ensure decimal points are dots and avoid locale issues
  # Run benchmark with perf to measure energy, suppress stdout (Results: line)
  LC_ALL=C perf stat -e power/energy-psys/ dune exec -- ./main.exe cold --elements "$elements" --finds "$finds" --runs "$runs" --domains "$i" > /dev/null 2> perf.out

  # Extract bench data from the generated CSV file (skip header line)
  if [ -f "$csv_file" ]; then
    bench_line=$(tail -1 "$csv_file")
  else
    bench_line="$i,N/A,N/A,N/A,N/A,N/A,N/A"
  fi

  # Extract Joules (remove commas if any)
  joules=$(grep "Joules" perf.out | awk '{print $1}' | tr -d ',')
  if [ -z "$joules" ]; then joules="N/A"; fi

  # Extract Seconds (remove commas if any) and convert to milliseconds
  seconds=$(grep "seconds time elapsed" perf.out | awk '{print $1}' | tr -d ',')
  if [ -z "$seconds" ]; then
    milliseconds="N/A"
  else
    milliseconds=$(echo "$seconds" | awk '{printf "%d", $1 * 1000}')
  fi

  echo "$bench_line,$joules,$milliseconds"
done

rm -f perf.out
