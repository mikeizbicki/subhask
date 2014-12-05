subhask
=======

This library provides an alternative prelude for Haskell.
The goal is to provide a type safe interface for working in subcategories of Hask.
This requires rewriting essentially all of the algebraic type classes, making them much more generic.

TODO:

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

2.

