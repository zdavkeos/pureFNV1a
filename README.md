
# pureFNV1a

## About

pureFNV1a is a implementation of the 32-bit FNV-1a hash algorithm
written in pure Haskell.  It includes an interface to the `crypto-api`
interface as well as a stand alone hashing function.  For more
information on the FNV-1a algorithm, see the
[Wikipedia page](http://en.wikipedia.org/wiki/Fowler_Noll_Vo_hash).
The original RFC is
[here](http://tools.ietf.org/html/draft-eastlake-fnv-03).

This module is loosely based on the code for the `pureMD5` package for
Haskell.  FNV-1a is not quite as great a hash algorithm as
[Murmur](https://sites.google.com/site/murmurhash/), but it gets the
job done and serves as a fine example of using the Haskell
`crypto-api`.

## Quick Start

To get started, run:

    cabal configure --enable-tests
    cabal build
    cabal test

This will configure, build and test the package.  If everything goes
alright, you can run the `dist/build/fnv1a/fnv1a` program.  Simple
pipe some bytes in over `stdin`, and the hash will be printed to `stdout`
(similar to the `md5sum` command).

