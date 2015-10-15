Secure ML Modules Compiler
==========================================

[![Build Status](https://travis-ci.org/sylvarant/secure-ml-compiler.svg?branch=Lite)](https://travis-ci.org/sylvarant/secure-ml-compiler) [![Coverage Status](https://coveralls.io/repos/sylvarant/secure-ml-compiler/badge.svg?branch=master&service=github)](https://coveralls.io/github/sylvarant/secure-ml-compiler?branch=master) 

### What is this repository for? ###

This repository contains the implementation of the
secure compiler for ModuleML described in the APLAS 2015 paper:`A Secure Compiler for ML Modules`.

### Setup ###
Set up the environment:
```bash
make setup
```
Compile the compiler:
```bash
make now
```
Compile and run the tests:
```bash
make test
```

### Repository Structure ###
* src/ : compiler source code
* lib/ : run-time environment for the compiled secure code
* tests/ : all compiler tests
    * interoperation : implementations of the low-level contexts that test the output of the compiler
    * timing : implementations of the low-level contexts used to obtain the experimental results
* log/ : stores logs of test runs
* out/ : stores the low-level objects and executables

### Scripts ###

- `compile` : a bash wrapper around the compiler
```bash
compile 0 <file> # compile securely
compile 1 <file> # comile naively
```

- `link` : a wrapper around the gcc linker to help combining modules and contexts
```bash
link <object> <file> [-DINSECURE]
```

- `test` : custom built testing mechanism

## License

[Artistic License 2.0](http://www.perlfoundation.org/artistic_license_2_0)
