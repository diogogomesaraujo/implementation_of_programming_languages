<br />
<div align="center">
  <h3 align="center">fun</h3>
  <p align="center">
      A core functional programming language implemented in OCaml.
  </p>
</div>

<!-- ABOUT THE PROJECT -->
## About

This repository contains a compiler from an extended call-by-value λ-Calculus to SECD-machine instructions and a stack machine that runs the compiled code. To learn more about the implementation read the [report](./report.pdf).
<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Getting Started

### Prerequisites

In order to run this project from source, you will need to have OCaml, `dune`, and `opam` installed. To install them you can follow the [OCaml Documentation](https://ocaml.org/install#linux_mac_bsd).

### Command-line Tool

You can execute programs written in files with the following command:
```bash
dune exec fun -- <file_path>
```

Or use the interactive environment with:
```bash
dune exec fun -- --repl
```

### Documentation

1. To compile the documentation run:

```bash
dune build @doc
```

2. Then open the documentation with:

```bash
open _build/default/_doc/_html/index.html 
```

<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Code Examples

There are several examples you can try in the [`examples`](./examples) folder such as:

1. Increment
```haskell
let incr =
  \x -> x + 1
in incr 1
```
2. Factorial

```haskell
let fact =
    fix (
        \g -> \x ->
            ifzero x 1 (x * (g (x - 1)))
    )
in fact 5
```

3. Fibonnaci Sequence

```haskell
let fib =
    fix (
        \g -> \x ->
            ifzero (x - 1) 1
                (ifzero x 1 ((g (x - 1)) + (g (x - 2))))
    )
in fib 10
```
4. Catalan Numbers
```haskell
let fact =
    fix (
        \g -> \x ->
            ifzero x 1 (x * g (x - 1))
    )
in let catalan =
    \x ->
        fact (2 * x) /
        (fact (x + 1) * fact x)
in catalan 10
```
<p align="right">(<a href="#readme-top">back to top</a>)</p>
