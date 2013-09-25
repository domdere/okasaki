# Okasaki

My Code-Along during my read through of Chris Okasaki's *Purely Functional Data Structures* ( *PFDS* )

## Contents

So far this library contains implementations of the following Chapters of *PFDS*.

### Chapter 3

Chapter 3 Deals with the following Data Structures (with the relevant modules listed):

-   Heaps ( [**Data.Heap**](./src/Data/Heap.hs "Data.Heap") )
    -   Leftist Heaps ( [**Data.Heap.Leftist**](./src/Data/Heap/Leftist.hs "Data.Heap.Leftist") )
        -   Height Biased ( [**Data.Heap.Leftist.HeightBiased**](./src/Data/Heap/HeightBiased.hs "Data.Heap.Leftist.HeightBiased") )
        -   Weight Biased ( [**Data.Heap.Leftist.WeightBiased**](./src/Data/Heap/WeightBiased.hs "Data.Heap.Leftist.WeightBiased") )
    -   Binomial Trees/Heaps
        -   Binomial Trees ( [**Data.Heap.Binomial.Tree**](./src/Data/Heap/Binomial/Tree.hs "Data.Heap.Binomial.Tree") )
        -   Binomial Heaps ( [**Data.Heap.Binomial.Heap**](./src/Data/Heap/Binomial/Heap.hs "Data.Heap.Binomial.Heap") )

## Building the project

The project must be "configured" at least once everytime `okasaki.cabal` changes, this can be done with:

    cabal configure

If you wish to run the unit tests you will have to run:

    cabal configure --enable-tests

Then finally build it with:

    cabal build

See `cabal build --help` for more build options.

## Running Unit Tests

**After** running `cabal build`, you can run the unit tests with the command:

    cabal test

## Adding Unit tests

Unit tests are written with [**doctest**] [doctest-github], for instructions on how to add unit tests
see the **doctest** [**User Guide**] [doctest-userguide].

[doctest-github]: https://github.com/sol/doctest-haskell "sol/doctest-haskell on GitHub.com"
[doctest-userguide]: https://github.com/sol/doctest-haskell/blob/master/README.markdown#usage "doctest Usage Guide"

