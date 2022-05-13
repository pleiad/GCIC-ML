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
    # This builds it in a sandboxed environment
    esy b dune build
    ```
4. Run it and test it out.
   ```bash
   esy run
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
## Resources
- [Gradualizing the Calculus of Inductive Constructions (paper)](https://dl.acm.org/doi/10.1145/3495528)