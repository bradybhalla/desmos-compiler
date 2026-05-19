# desmos-compiler

## Setup instructions

- Install dependencies with `opam install . --deps-only`
- Run all tests with `dune runtest`

## Usage


The program can be run from the command line as follows:
```sh
# print js output
dune exec desmos-compiler -- desmos <file>

# run with emulator
dune exec desmos-compiler -- emulator <file> -reg <output register>

# print sexp of intermediate language
dune exec desmos-compiler -- debug <file>
```

You can also pass `-help` to any command for more information / additional arguments.
