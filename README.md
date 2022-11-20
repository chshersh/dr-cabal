# dr-cabal

[![GitHub CI](https://github.com/chshersh/dr-cabal/workflows/CI/badge.svg)](https://github.com/chshersh/dr-cabal/actions)
[![Hackage](https://img.shields.io/hackage/v/dr-cabal.svg?logo=haskell)](https://hackage.haskell.org/package/dr-cabal)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

`dr-cabal` is a CLI tool for profiling Haskell dependencies build times.

![dr-cabal demo](https://raw.githubusercontent.com/chshersh/dr-cabal/main/images/dr-cabal-demo.gif)

> ℹ️ **DISCLAIMER:** This project is developed and maintained in
> free time by volunteers. The development may continue for decades or
> may stop tomorrow. You can use
> [GitHub Sponsorship](https://github.com/sponsors/chshersh) to support
> the development of this project.

## Install

`dr-cabal` is a CLI tool written in Haskell and can be installed
either from Hackage or from this repository.

### Prerequisites

To start using `dr-cabal`, make sure you have the required version of
`cabal-install` (a Haskell build tool) and GHC (a Haskell compiler)
installed:

* [Cabal ⩾ 3.6.2.0](https://www.haskell.org/cabal/)
* [GHC ⩾ 9.0.2](https://www.haskell.org/downloads/)

### Hackage

To install the latest version of `dr-cabal` from
[Hackage](https://hackage.haskell.org/package/dr-cabal),
follow these steps:

1. Update Hackage index:

    ```shell
	cabal update
	```

2. Build `dr-cabal` from Hackage and copy the resulting executable
   into the desired location (e.g. `$HOME/.local/bin`):

    ```shell
    cabal install dr-cabal \
        --install-method=copy \
        --overwrite-policy=always \
        --installdir=$HOME/.local/bin
	```

> ℹ️ **NOTE:** Make sure the `$HOME/.local/bin` directory or the
> directory of your choice is listed in `$PATH`.

### Sources

To build the latest version of `dr-cabal` from sources,
follow these steps:

1. Clone this repository:

    ```shell
    git clone https://github.com/chshersh/dr-cabal.git
	cd dr-cabal
	```

2. Build the package:

    ```shell
	cabal build
	```

3. Copy executable to the desired location:

    ```shell
    cp $(cabal list-bin exe:dr-cabal) ~/.local/bin/dr-cabal
	```

> ℹ️ **NOTE:** Make sure the `~/.local/bin` directory or the
> directory of your choice is listed in `$PATH`.

## Quick start guide

Run the following command to view interactive profiling report:

```shell
cabal --store-dir=$(mktemp -d) build --dependencies-only all | dr-cabal profile
```

### Explanation

This section explains the above command:

1. `dr-cabal` watches the output of the `cabal build` command to
   produce the profiling report. Step into the directory of the
   Haskell project you want to profile and pipe the output of
   `cabal build` to `dr-cabal profile`.
2. Currently, `dr-cabal` can profile only dependencies. So you can
   pass the `--dependencies-only` to avoid extra wait.
3. `cabal` caches built dependencies. You can specify a custom
   directory for storing build artifacts using the `--store-dir` flag
   to build the dependencies anew.
4. The `$(mktemp -d)` command generates a temporary directory so you
   can run the build time profiler in an isolated location.

> ⚠️ **WARNING:** To get meaningful results, including downloading
> of packages, the `dr-cabal watch` command needs to be run when
> none of the dependencies are build (i.e. with cold cabal
> cache). If you've already build you project, including
> dependencies, you can purge global Cabal cache using the
> following command:
>
> ```shell
> rm -rf ~/.cabal
> ```

## Usage

> ℹ️ In this section, a more verbose `cabal-install` command from
> "Quick start guide" is replaced with shorter `cabal build`.

`dr-cabal` supports profiling of documentation as well, you only need
to pass relevant flags to `cabal build`:

```shell
cabal build --enable-documentation --haddock-all | dr-cabal profile
```

To cache the profiling results in JSON (and avoid building the project
again), use the `--output` flag:

```shell
cabal build | dr-cabal profile --output=my_file.json
```

Once you successfully produced a JSON file with all the recorded
steps, run the following command to pretty-print the profiling output:

> ⚠️ **WARNING:** For better results, make your terminal full-screen.

```shell
dr-cabal profile --input=my_file.json
```

You'll see the output like on the image below:

![dr-cabal bigger example](https://raw.githubusercontent.com/chshersh/dr-cabal/main/images/dr-cabal-itself.png)
