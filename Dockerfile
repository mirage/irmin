FROM ocaml/opam:ubuntu-ocaml-5.3

# Install system dependencies
RUN sudo apt-get update && sudo apt-get install -y \
    libffi-dev \
    gnuplot-x11 \
    libgmp-dev \
    libssl-dev \
    pkg-config \
    qemu-system

# Set working directory
WORKDIR /home/opam/irmin

# Clone the repository
#RUN git clone -b spice https://github.com/mirage/irmin.git .
COPY . .

# Pin the package
RUN opam pin -yn .

# Install dependencies (only)
RUN opam install -y --deps-only --with-test .
