[![codecov](https://codecov.io/gh/pleiad/GCIC/branch/cast_cic/graph/badge.svg?token=WT2VULQV1A)](https://codecov.io/gh/pleiad/GCIC)

# GCIC
Prototype implementation of GCIC

## Setup
First off, you need to clone the repository.
```bash
git clone https://github.com/pleiad/GCIC.git
# or alternatively
git clone git@github.com:pleiad/GCIC.git
```
We have added some tools to facilitate the development of Ocaml code. However, 
you are free to use whatever you want. We'll mention the ones we are currently using and that we have setup for the project.

### Esy 
[Esy](https://esy.sh/) is a package manager for Reason and Ocaml, but also provides 
some sandboxing capabilities. You can start your own local environment just for this 
project, without needing Opam switches or contaminating your existing ones (Esy interacts well with opam and dune in any case!). 
#### Setup
1. To start, install esy. This installation path requires [npm](https://www.npmjs.com/), if you need some 
other option, consider checking [esy's documentation](https://esy.sh/docs/en/getting-started.html).
    ```shell
    npm install -g esy
    ```
2. Install dependencies by running the following command in your shell.

    ```bash
    esy
    ```
3. Build the project.
    ```bash
    esy make
    ```
4. Run it and test it out.
   ```bash
   esy repl
   ```

Now you should be able to play with GCIC or start developing :tada:!


#### Additional commands
We have some additional commands to make it easier to develop in Ocaml.
1. Hot rebuild (this keeps on running and building the project automatically).
   ```bash
   esy watch
   ```
2. Run the test suite.
    ```bash
    esy test
    ```
3. Run the tests with coverage and generate a [Bisect](https://github.com/aantron/bisect_ppx) report.
    ```bash
    # This generates a _coverage folder, where you can then open the index.html file
    esy coverage && esy report
    ```
4. Run the formatter and update files
    ```bash
    esy format
    ```

#### Better REPL
For a better repl experience install [rlwrap](https://github.com/hanslub42/rlwrap). This will provide you with multiple features, such as: auto-completion, history, search, etc. (check rlwrap's documentation for more info).
```bash
# OS X 
brew install rlwrap
# Linux
apt install rlwrap
```
Then you can simply run the program as before `esy repl`.
You should see the changes in effect immediately. 

### Nix 
[Nix](https://nixos.org/) is a declarative package manager.

#### Setup
1. To start, install nix. You can check [nix's documentation] (https://nix.dev/tutorials/install-nix) for more information.
    ```shell
    # Linux
    sh <(curl -L https://nixos.org/nix/install) --daemon
    # macOS
    sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume --daemon
    # Windows (WSL2)
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
    ```

3. Launch the development environment.
    ```bash
    nix-shell
    ```
4. Run it and test it out.
   ```bash
   gcic-repl
   ```

Now you should be able to play with GCIC or start developing :tada:!


#### Additional commands
We have some additional commands to make it easier to develop in Ocaml.
1. Hot rebuild (this keeps on running and building the project automatically).
   ```bash
   dune-watch
   ```
2. Run the test suite.
    ```bash
    dune-test
    ```
3. Run the tests with coverage and generate a [Bisect](https://github.com/aantron/bisect_ppx) report.
    ```bash
    # This generates a _coverage folder, where you can then open the index.html file
    dune-coverage
    ```
4. Run the formatter and update files
    ```bash
    dune-format
    ```

## Resources
- [Gradualizing the Calculus of Inductive Constructions (paper)](https://dl.acm.org/doi/10.1145/3495528)