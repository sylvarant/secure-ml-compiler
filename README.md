# README #

### What is this repository for? ###

Ocaml implementation of a secure compiler for ModuleML.


### Setup ###
$ make setup
$ make now
$ make test

### Repository Structure ###
* src/ : compiler source code
* lib/ : run-time environment for the compiled secure code
* tests/ : all compiler tests
    * interoperation : implementations of the low-level contexts that test the output of the compiler
    * timing : implementations of the low-level contexts used to obtain the experimental results
* log/ : stores logs of test runs
* out/ : stores the low-level objects and executables

### Scripts ###

$ compile : a wrapper around the compiler
    - compile 0 <file> : compile securely
    - compile 1 <file> : comile naively

$ link : a wrapper around the gcc linker to help combining modules and contexts
    - link <object> <file> [-DINSECURE]

$ test : custom built testing mechanism


