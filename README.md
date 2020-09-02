# Sys: A Static/Symbolic Tool for Finding Good Bugs in Good (Browser) Code

# Install

## Install dependencies

- llvm-9
- llvm-9-tools
- [boolector](https://github.com/Boolector/boolector) configured with
  `--shared` option. See the `build()` and `package()` functions in [this
  file](https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=boolector-git)
  as an example of how to install boolector after you clone it.
- The Haskell tool [Stack](https://docs.haskellstack.org/en/stable/README/)

## Build project

Once you have all the dependencies installed you should be able to just build the tool:

```
stack build
```

# Test

Once you built the tool you can build and run our tests with:

```
stack test
```

This will run a more-or-less full version of our test suite, along with regression tests for every bug that we list in the paper. The suite takes a little over two minutes on laptop with 64GB of RAM and 8 threads. All tests with one exception---a bug whose source we're having trouble tracking down---should pass. If anything else fails, try re-running the tests; the solver may have timed out (this hasn't happened on our machines, but since we can't give you a login for annonymity, its a possibility that it will happen on your machine). 

If you just want to reproduce the paper results and nothing else, run:

```
stack test --ta '-p End-to-end'
```

# Run

Once you built the tool you can now use it to find bugs!

```
stack exec sys
```
The tool takes several options:

```
  -d DIR    --libdir=DIR   directory (or file) to analyze
  -e EXTN   --extn=EXTN    file extension
  -c CHECK  --check=CHECK  checker to run
```

- The `-d` option is used to specify the directory (containing the LLVM files) or a single LLVM file.
- The `-e` option is used to specify the extension of files to check. This is
  useful when building your project with different optimizations levels (e.g.,
  `.ll-O0` for debug build with `-O0` and `.ll-O0_p` for production).
   -  `ll` matches all `*.ll` files
   -  `O0` matches all `*.ll-O0` and `*.ll-O0_p` files
   -  `O1` matches all `*.ll-O1` and `*.ll-O1_p` files
   -  `O2` matches all `*.ll-O2` and `*.ll-O2_p` files
   -  `O3` matches all `*.ll-O3` and `*.ll-O3_p` files
   -  `Og` matches all `*.ll-Og` and `*.ll-Og_p` files
   -  `Os` matches all `*.ll-Os` and `*.ll-Os_p` files
   -  `Oz` matches all `*.ll-Oz` and `*.ll-Os_z` files
   -  `prod` matches all `*_p` files
   -  `any` matches all files
- The `-c` option is used to specify the checker to run, one of:
   - `uninit`: Uninitialized memory checker
   - `heapoob`: Malloc OOB checker
   - `concroob`: Negative index OOB checker
   - `userinput`: User input checker


## Example

To find the uninitialized memory access bug that our tool found in Firefox's Prio library:

```
$ stack exec sys -- -c uninit -e prod -d ./test/Bugs/Uninit/Firefox/serial.ll-O2_p
```

The tool flags two bugs. Let's look at the first:

```
Stack uninit bug
Name "serial_read_mp_array_73"
in 
Name "serial_read_mp_array"
path-to-file
[UnName 4,UnName 71]
```

If you inspect the [serial_read_mp_array()](./test/Bugs/Uninit/Firefox/serial.ll-O2_p#L1528) function, the buggy block path is `%4` (the first block) to `%71`,where we use [`%73`].

# Directory structure

```
├── app            -- Executable used to run the checkers
├── src
│   ├── Checkers   -- Static and symbolic checkers
│   ├── Control    -- Logging helpers
│   ├── LLVMAST    -- LLVM AST interface
│   ├── InternalIR -- Internal IR used to represent paths for both static and symex
│   ├── Static     -- Static checker DSL
│   └── Symex      -- Symbolic DSL and execution engine
└── test           -- Tests
```
