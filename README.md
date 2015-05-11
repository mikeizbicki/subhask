#subhask

This library provides a type safe interface for working in arbitrary subcategories of Hask.
This is a big task that requires rethinking much of the haskell prelude. 
The motivation for this project is to make *fast* numeric applications *easy to develop*.

This library is still very experimental, but current features include:

<ol>
<li> An alternative prelude for GHC that fixes many of the problems with the type class hierarchy.
The goal of this class hierarchy is to align as closely as possible to the way mathematicians think.
It can be thought of as a greatly expanded (but more principled) version of <a href="https://github.com/snoyberg/classy-prelude">classy-prelude</a>.

<p>Some relatively uncontroversial changes include:
<ol>
  <li> Refactoring the `Num` class into its components (`Semigroup`,`Monoid`,`Group`,...); this let's us support many more numeric types than Prelude can
  <li> Adding a `VectorSpace` hierarchy; this makes working with matrices in the HMatrix library much more convenient
  <li> Refactoring the `Ord` class into its components, including support for partial orders, lattices, and more exotic ordered structures
  <li> Refactoring the container-like classes (this is still in progress)
  <li> Refactoring the `Category` and `Monad` classes (also still a work in progress)
</ol>
Some more controversial changes include:
<ol>
  <li> Every type has an associated `Logic` that may be non-classical; this lets us perform equality on every time---including arbitrary functions---for certain non-classical notions of equality
  <li> A subtyping mechanism; subhasks's notion of subtypes is different than what I think most haskeller's want when they say they wish haskell supported subtyping, but it is useful in the context of our class hierarchies
  <li> A dependence on essentially every GHC extension
  <li> (even worse) a dependence on template haskell for syntactic sugar that makes working with the new class hierarchies tolerable
</ol>
See the [src/SubHask/Algebra.hs](src/SubHask/Algebra.hs) file for details on algebraic structures; [src/SubHask/Category.hs](src/SubHask/Category.hs) file for details on categorical structures; and [src/SubHask/Monad.hs](src/SubHask/Monad.hs) file for details on monadic structures.


<li> With many new classes come many new laws, and subhask provides template haskell functions to make enforcing these laws easier.
The goal is that every time you create an instance of a class, template haskell automatically generates a test suite for your instance.
These tests are not yet fully automated (due to some limitations in the current implementation of template haskell), but see the files [src/SubHask/TemplateHaskell/Test.hs](src/SubHask/TemplateHaskell/Test.hs) and [test/TestSuite.hs](test/TestSuite.hs) for the current implementation.

<li> Due to the massive changes in the class hierarchy, other libraries will not work out-of-the box with subhask.
There is currently a small compatibility layer for the `base`, `containers`, `vector`, and `hmatrix` libraries.
There are also some template haskell tools to automatically derive subhask classes based on prelude classes.
But this area needs significant work.
Probably the easiest way to contribute to/get started with subhask is to pick your favorite haskell library and write a compatibility layer.
</ol>

## TODO

There's a lot left to do.
These are just some random thoughts I wrote down so I wouldn't forget.
There's plenty of smaller `FIXME` comments throughout the code.

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


