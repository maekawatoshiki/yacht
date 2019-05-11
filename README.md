# Yacht

[![CircleCI](https://circleci.com/gh/maekawatoshiki/yacht.svg?style=shield)](https://circleci.com/gh/maekawatoshiki/yacht)
[![codecov](https://codecov.io/gh/maekawatoshiki/yacht/branch/master/graph/badge.svg)](https://codecov.io/gh/maekawatoshiki/yacht/branch/master)
[![](http://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

ECMA-335 (Common Language Infrastructure, .NET) implementation written in Rust.

*Just for fun*

# Building from Source

## Building on Linux-like systems

1. Install Rust

Run the command below and follow the onscreen instructions. 

```sh
curl https://sh.rustup.rs -sSf | sh
```

2. Use Rust Nightly

```sh
rustup override set nightly
```

3. Install dependencies
  - LLVM 6.0
  - (Other packages as necessary...)

```sh
# e.g. Ubuntu or Debian
apt-get install llvm-6.0
```

4. Test 

```sh
cargo test
```

5. Run

```sh
cargo run --release examples/XXX.exe
```

