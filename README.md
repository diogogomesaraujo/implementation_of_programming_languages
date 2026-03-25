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

This repository contains interpreters and compilers for a simple functional programming language called `fun` for Implementation of Programming Languages class of my Master's degree at FCUP.
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
