# Nix templates for Common Lisp projects

This repository contains a Nix template for Common Lisp projects.

## Usage

### Run entrypoint

The entrypoint of this project is `main` function.
You can run it with the following command:

```sh
nix run .#main-sbcl
```

- `.#main-sbcl`: Run the entrypoint using SBCL.
- `.#main-ecl`: Run the entrypoint using ECL.
- `.#main-abcl`: Run the entrypoint using ABCL.
- `.#main-ccl`: Run the entrypoint using CCL.
- `.#main-mkcl`: Run the entrypoint using MKCL.
- `.#main-cmucl_binary`: Run the entrypoint using CMUCL.
- `.#main-clasp-common-lisp`: Run the entrypoint using Clasp.

### Run tests

The tests of this project are located in the `test` directory.
You can run them with the following command:

```sh
nix run .#test-sbcl
```

We use [FiveAM](https://github.com/lispci/fiveam) for testing.

- `.#test-sbcl`: Run all tests using SBCL.
- `.#test-ecl`: Run all tests using ECL.
- `.#test-abcl`: Run all tests using ABCL.
- `.#test-ccl`: Run all tests using CCL.
- `.#test-mkcl`: Run all tests using MKCL.
- `.#main-cmucl_binary`: Run all tests using CMUCL.
- `.#main-clasp-common-lisp`: Run all tests using Clasp.

### Run REPL

You can run a REPL with the following command:

```sh
nix develop -c sbcl # ecl, abcl, or ccl, mkcl, lisp, clasp
```

To load the project, you can use the following code:

```lisp
(require :uiop)
(load (uiop:getenv "ASDF"))
(require :fibonacci)
```

### Use library

This project exports the following libraries:

- `packages.lib-sbcl`: library for SBCL.
- `packages.lib-ecl`: library for ECL.
- `packages.lib-abcl`: library for ABCL.
- `packages.lib-ccl`: library for CCL.
- `packages.lib-mkcl`: library for MKCL.
- `packages.lib-cmucl_binary`: library for CMUCL.
- `packages.lib-clasp-common-lisp`: library for Clasp.

## Development

First, you need to rename the project name.
We attached the rename script for this purpose.

```sh
vim ./rename.sh # Edit the project name
./rename.sh
```

Then, you might update the dependency (`lispLibs`)
and version (`version`) of the project in the `flake.nix` file.

Now you can start developing your project.
Note that this project uses _package-inferred-system_.

## License

This project is inspired by
[Comamoca/nix-template](https://github.com/Comamoca/scaffold/tree/main/cl-nix),
which is licensed under the CC0 1.0 Universal license.

This projects is licensed under the following license:

>
> MIT No Attribution
>
> Copyright 2024 Masaya Taniguchi
>
> Permission is hereby granted, free of charge, to any person obtaining a copy of this
> software and associated documentation files (the "Software"), to deal in the Software
> without restriction, including without limitation the rights to use, copy, modify,
> merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
> permit persons to whom the Software is furnished to do so.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
> INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
> PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
> HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
> OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
> SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

