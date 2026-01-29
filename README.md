# MDE Parser

An extensible OCaml parser for a Markdown-inspired markup language. This project features a modular architecture where syntax elements (features) are registered into a central registry, allowing for easy expansion and customization of the parsing engine.

## Features

* **Extensible**: Add new syntax elements by implementing the `FEATURE` module type.
* **HTML and TeX**: Designed for HTML and TeX rendering (TeX support in progress).
* **Specific syntax**: Supports metadata (YAML), environments (theorems, proofs, …), and standard Markdown styling.

---

## Usage

You can integrate the parser into your own OCaml projects by using the `Mde_parser` module:

```ocaml
let mde_input = "**Hello** MDE!" in
let (html_output, metadata) = Mde_parser.parse_mde mde_input in
print_endline html_output

```

---

## Development


Ensure you have [OCaml](https://ocaml.org/) and [opam](https://opam.ocaml.org/) installed.

To install dependencies and build the project:

```bash
make default

```

## Documentation

The project uses `odoc` to generate API documentation. To build the docs, run

```bash
make doc

```
then open `_docs/mde_parser/index.html` in your browser.
