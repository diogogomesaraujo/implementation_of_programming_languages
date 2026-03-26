<br />
<div align="center">
  <h3 align="center">fun</h3>
  <p align="center">
      A core functional programming language implemented in OCaml.
  </p>
</div>

<div align="center">
    <img src="./assets/cam.gif" alt="Demo">
</div>

<!-- ABOUT THE PROJECT -->
## About

This repository contains a compiler from an extended call-by-value λ-Calculus to SECD-machine instructions.
<p align="right">(<a href="#readme-top">back to top</a>)</p>

## Getting Started

### Prerequisites

In order to run this project from source, you will need to have OCaml, `dune`, and `opam` installed. To install them you can follow the [OCaml Documentation](https://ocaml.org/install#linux_mac_bsd).

### Runnable Examples

There are several code implementations in the `examples` directory you can run by changing the path in the `main.ml` executable.

```OCaml
let () =
  let fib = parse_from_file "examples/catalan.fn" |> Option.get in
  match compile fib [] |> run with
  | Int x -> Printf.printf "%d\n" x
  | _ -> failwith "Couldn't reach an integer value."
```

To run execute the following command:
```bash
dune exec fun
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

1. Factorial

```
let fact =
    fix (
        \g -> \x ->
            ifzero x 1 (x * (g (x - 1)))
    )
in fact 5
```

2. Fibonnaci Sequence

```
let fib =
    fix (
        \g -> \x ->
            ifzero (x - 1) 1
                (ifzero x 1 ((g (x - 1)) + (g (x - 2))))
    )
in fib 10
```
3. Catalan Numbers
```
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
