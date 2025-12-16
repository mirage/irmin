all: bench.png

bench.csv: bench.sh
	./bench.sh -e 100 -f 100 -r 10 > bench.csv

bench.png: bench.csv bench.gp
	gnuplot bench.gp

clean:
	rm -f bench.csv bench.png *~
