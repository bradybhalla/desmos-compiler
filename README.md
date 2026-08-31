# desmos-compiler

A compiler for a simple imperative language that allows it to execute in the Desmos graphing calculator. Desmos has an "action" feature that allows it to modify variables when the action is executed. This compiler turns a program into a giant piecewise Desmos action that simulates the execution as it is repeatedly executed.

## Example Usage

- Compile a program
    ```sh
    dune exec desmos-compiler -- desmos test/programs/collatz.sexp
    ```
- Open [www.desmos.com/calculator](https://www.desmos.com/calculator) and paste the output of the program into the JS console.
- Run the program by EITHER
    - repeatedly clicking the "->" next to equation 1 until the value of equation 3 becomes "-1".
    - OR automate this by clicking "+" in the top left, select "ticker", type "M_ain" into the first field, and click the play button next to it.
- To reset the program, click "->" next to equation 2.

## CLI

```sh
# print js output for running in Desmos
dune exec desmos-compiler -- desmos <file>

# run with emulator
dune exec desmos-compiler -- emulator <file> -reg <output register>

# print sexp of intermediate language(s)
dune exec desmos-compiler -- debug <file>
```

You can also pass `-help` to any command for more information / additional arguments.

## Setup

Install dependencies with `opam install . --deps-only` or use the Nix flake's dev shell.

## Testing

This project uses expect tests. Run with `dune runtest`. If something changes  you can accept the new output with `dune promote`.
