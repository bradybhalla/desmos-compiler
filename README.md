# desmos-compiler

## Setup

Install dependencies with `opam install . --deps-only`

## Usage

The program can be run from the command line as follows:
```sh
# print js output for running in Desmos
dune exec desmos-compiler -- desmos <file>

# run with emulator
dune exec desmos-compiler -- emulator <file> -reg <output register>

# print sexp of intermediate language(s)
dune exec desmos-compiler -- debug <file>
```

You can also pass `-help` to any command for more information / additional arguments.

## Documentation

Run `dune build @doc-private` to generate documentation and navigate to it in the _build folder. This should have the most up-to-date information about the different languages and passes.

## Testing

This project uses expect tests. Run with `dune runtest`. If something changes  you can accept the new output with `dune promote`.
