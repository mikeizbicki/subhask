#subhask

This library provides a type safe interface for working in arbitrary subcategories of Hask.
In practice, this means subhask is an alternative prelude for GHC.
The motivation for this project is to make fast numeric applications easy to develop.

This library is still very experimental, but current features include:

1.
An alternative prelude for GHC that fixes many of the problems with the type class hierarchy.
The goal of this class hierarchy is to align as closely as possible to the way mathematicians think.
It can be thought of as a greatly expanded (but more principled) version of [classy-prelude](https://github.com/snoyberg/classy-prelude).
Many of the changes will be rather controversial, but there are too many to list here.
See the src/SubHask/Algebra.hs file for details on algebraic structures; src/SubHask/Category.hs file for details on categorical structures; and src/SubHask/Monad.hs file for details on monadic structures.

2.
With many new classes come many new laws, and subhask provides template haskell functions to make enforcing these laws easier.
The goal is that every time you create an instance of a class, template haskell automatically generates a test suite for your instance.
These tests are not yet fully automated (due to some limitations in the current implementation of template haskell), but see the files src/SubHask/TemplateHaskell/Test.hs and test/TestSuite.hs for the current implementation.

4.
Due to the massive changes in the class hierarchy, other libraries will not work out-of-the box with subhask.
There is currently a small compatibility layer for the `base`, `containers`, `vector`, and `hmatrix` libraries.
There are also some template haskell tools to automatically derive subhask classes based on prelude classes.
But this area needs significant work.
Probably the easiest way to contribute to/get started with subhask is to pick your favorite haskell library and write a compatibility layer.

## TODO

1. Add tree metrics.
There's two hurdles here.
The first is implementing the metric.
There's many different algorithms to choose from, and none of them are trivial.
The second problem is that we need a generic tree type class in order to use our tree metrics.
What's the right way to do this?
Trees are related to magmas, so should this touch the algebra hierarchy?
But trees are also related to graphs, and ideally they would use the same interface;
in particular, there should be a graph class, and tree should be a subclass of graph.
But this could be a really deep rabbit hole, and might end up being a LOT of work.

2. Metric spaces are part of a huge hierarchy of ways to measure distance.
For example, there are premetric spaces and bregman divergences on the smaller side; and additive metrics and translation invariant metrics on the larger side.
There are a LOT of potential classes like this, and the problem is how do we decide which ones are worth the cost of implementing?
Also, what are the efficiency trade offs we would get?


