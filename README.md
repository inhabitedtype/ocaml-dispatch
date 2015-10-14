# ocaml-dispatch

ocaml-dispatch provides a basic mechanism for dispatching a request to a
handler based on hierarchical path names conventionally found in URIs. It can be
used both for dispatching requests in a server, as well as handing changes to
hierarchical fragments in a client-side application.

[![Build Status](https://travis-ci.org/inhabitedtype/ocaml-dispatch.svg?branch=master)](https://travis-ci.org/inhabitedtype/ocaml-dispatch)

## Installation

Install the library and its depenencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install dispatch
```

## Development

To install development versions of the library, pin the package from the root
of the repository:

```bash
opam pin add .
```

You can install the latest changes by commiting them to the local git
repository and running:

```bash
opam upgrade dispatch
```

For building and running the tests during development, you will need to install
the `oUnit` package and reconfigure the build process to enable tests:

```bash
opam install oUnit
./configure --enable-tests
make && make test
```

## License

BSD3, see LICENSE file for its text.
