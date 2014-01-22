# Murmerl - Murmur3 hash as Erlang NIF

[![Build Status](https://travis-ci.org/sideshowcoder/murmerl.png?branch=master)](https://travis-ci.org/sideshowcoder/murmerl)

This is a port of the Murmur3 hash function. Murmur3 is a non-cryptographic
hash, designed to be fast and excellent-quality for making things like hash
tables or bloom filters. This is a wrapper in for Erlang using NIFs.

## Prerequisits
The code was developed using Erlang/OTP R1603 with the bundled version of rebar
and murmur3.

## Building and testing
Build and test is done using rebar

    $ ./rebar compile
    $ ./rebar eunit

## Usage
Build and load via

    $ erl -pa ebin
    Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10]
    [hipe] [kernel-poll:false] [dtrace]

    Eshell V5.10.4
    1> murmerl:murmur3_x86_32("abc", 32).
    <<138,91,68,123>>

## Functions

### murmur3_x86_32(Subject, Seed) -> Binary
Types:
    Subject = string()
    Seed = integer()

creates a 32Bit binary, from the input string using Int as a seed. The C code
is optimized for x86, but works on x64 as well.

### murmur3_x86_128(String, Int) -> Binary
Types:
    Subject = string()
    Seed = integer()

creates a 128Bit binary, from the input string using Int as a seed. The C code
is optimized for x86, but works on x64 as well.

### murmur3_x64_128(String, Int) -> Binary
Types:
    Subject = string()
    Seed = integer()

creates a 128Bit binary, from the input string using Int as a seed. The C code
is optimized for x64, but works on x86 as well.

## License
Murmur3 port in C was done by Peter Scott and is available on
[github](https://github.com/PeterScott/murmur3), Erlang wrapper was done by
Philipp Fehre and is available under the MIT License.

