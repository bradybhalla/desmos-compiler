# desmos-compiler

## Setup instructions

- Install dependencies with `opam install . --deps-only`
- Run all tests with `dune runtest`

## Usage

Install with `opam install .`

The program can be run from the command line as follows:
```sh
# print js output
desmos-compiler desmos <file>

# run with emulator
desmos-compiler emulator <file> -reg <output register>

# print sexp of intermediate language
desmos-compiler debug <file> -pass <final pass>
```

You can also pass `-help` to any command for more information.
