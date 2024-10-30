# Nix templates for Common Lisp projects

This repository contains a Nix template for Common Lisp projects.

## Usage

### Run entrypoint

The entrypoint of this project is `main` function.
You can run it with the following command:

```sh
nix run
```

### Run tests

The tests of this project are located in the `test` directory.
You can run them with the following command:

```sh
nix run .#test
```

We use [Parachute](https://github.com/Shinmera/parachute) for testing.
Thanks to [Shinmera](https://github.com/Shinmera) for this great library.

### Run REPL

You can run a REPL with the following command:

```sh
nix develop
sbcl # `rlwrap sbcl` for readline support
```

To load the project, you can use the following code:

```lisp
(require :uiop)
(load (uiop:getenv "ASDF"))
(require :fibonacci)
```

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

