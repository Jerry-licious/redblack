# Red Black Trees in OCaml

For our final project in COMP 302 (Functional Programming), we have decided to implement the Red Black Tree data structure.

## Setting up
First, install OCaml. We do so by installing the OCaml Package Manager (opam). The steps below are taken from the [CS3110 Textbook](https://cs3110.github.io/textbook/chapters/preface/install.html)
### Installing OPAM
On Mac, install it through [Homebrew](https://brew.sh/)
```bash
brew install opam
```
On Linux, install it through apt, or your package manager of choice
```bash
sudo apt install opam
```
On Windows, setup [WSL](https://learn.microsoft.com/en-us/windows/wsl/install) and refer to the step above.

## Configuring OPAM
Run:
```
opam init --bare -a -y
opam update
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat
```

## Building
We are currently using `dune` as our package manager.
To build the project, simply run `dune build`.
To test the project, run `dune test`
To run the executable, run `dune exec redblack`