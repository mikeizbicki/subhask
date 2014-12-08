#subhask

This library provides an alternative prelude for Haskell.
The goal is to provide a type safe interface for working in subcategories of Hask.
Achieving this goal requires rewriting essentially all of the algebraic type classes, making them much more generic.

Conventions:

1. A function followed by a single quote is either strict or a derivative

2. A function followed by an underscore is a more generic version

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

2. Metric spaces are part of a huge hierarchy of ways to measure distance.
For example, there are premetric spaces and bregman divergences on the smaller side; and additive metrics and translation invariant metrics on the larger side.
There are a LOT of potential classes like this, and the problem is how do we decide which ones are worth the cost of implementing?
Also, what are the efficiency trade offs we would get?


