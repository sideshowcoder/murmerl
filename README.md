# Murmerl - Murmur3 hash as Erlang NIF

[![Build Status](https://travis-ci.org/sideshowcoder/murmerl.png?branch=master)](https://travis-ci.org/sideshowcoder/murmerl)

This is a port of the Murmur3 hash function. Murmur3 is a non-cryptographic
hash, designed to be fast and excellent-quality for making things like hash
tables or bloom filters. This is a wrapper in for Erlang using NIFs.

## Prerequisits
The code was developed using Erlang/OTP R1603 with the bundled version of rebar
and murmur3.

## Building and testing
Build and test is done using rebar, and make. To build an test run

    $ make all

To run only the tests run

    $ make test

Murmerl contains dialyzer spec, the first run will create the PLT file which can
take a while.

    $ make dialyzer

Run typer to output the -spec

    $ make typer

## Usage
Build and load via

    $ erl -pa ebin
    Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10]
    [hipe] [kernel-poll:false] [dtrace]

    Eshell V5.10.4
    1> murmerl:murmur3_x86_32("abc", 32).
    <<138,91,68,123>>

## Functions

### murmur3_32(Value, Seed) -> Binary
Types:
    Value = string()
    Seed = integer()

creates a 32Bit binary, from the input string using Int as a seed. The C code
is optimized for x86, but works on x64 as well.

### murmur3_128(Value, Seed) -> Binary
Types:
    Value = string()
    Seed = integer()

creates a 128Bit binary, from the input string using Int as a seed. The C code
is optimized for x86, but works on x64 as well.

### murmur3_128(Value, Seed, Arch) -> Binary
Types:
    Value = string()
    Seed = integer()
    Arch = x86 | x64

creates a 128Bit binary, from the input string using Int as a seed. It allows to
select the optimized code for the archtecture, so either x64 or x86.

## License
Murmur3 port in C was done by Peter Scott and is available on
[github](https://github.com/PeterScott/murmur3), Erlang wrapper was done by
Philipp Fehre and is available under the MIT License.

