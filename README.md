# Why Ka?

There just no any reasons (except of lack of stable compiler implementation (any compiler implementation, actually), standard library, LSP's, debuggers, versioning, and even a minimal language standard/documentation, actually) for you and your team to don't use Ka as a foundation of your next long term project.

# Inspirations

Ka language design is highly inspired by Rust.

# Design

As we know, Rust language removed all annoying excess functionality from C++ (object-oriented-like classes, exceptions, etc.), so Ka language went event further and removed all annoying excess functionality from Rust (traits, borrow checking, etc.) to make language even more comfortable and controlled.

> [!NOTE]
> These improvements actually resulted a fun thing - Ka is just C with Rust-like syntax and dietic portion of syntactic sugar added.

# Standard library

There is no standard library for Ka yet :(

# Compiler

There is three Ka compiler implementations planned, so there are their brief descriptions:

## KLC-RS
Rust and LLVM - based implementation of Ka compiler, is under active development and currently occupies this repo.
## KLC_C
C-based implementation, will be built as a ........................
## KLC
Main, Ka-based implementation, isn't even planned yet.
