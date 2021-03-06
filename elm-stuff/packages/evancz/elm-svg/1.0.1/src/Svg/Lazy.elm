module Svg.Lazy
    (lazy, lazy2, lazy3
    ) where

{-| Since all Elm functions are pure we have a guarantee that the same input
will always result in the same output. This module gives us tools to be lazy
about building `Svg` that utilize this fact.

Rather than immediately applying functions to their arguments, the `lazy`
functions just bundle the function and arguments up for later. When diffing
the old and new virtual DOM, it checks to see if all the arguments are equal.
If so, it skips calling the function!

This is a really cheap test and often makes things a lot faster, but definitely
benchmark to be sure!

@docs lazy, lazy2, lazy3
-}

import Svg (Svg)
import VirtualDom


lazy : (a -> Svg) -> a -> Svg
lazy =
    VirtualDom.lazy

lazy2 : (a -> b -> Svg) -> a -> b -> Svg
lazy2 =
    VirtualDom.lazy2

lazy3 : (a -> b -> c -> Svg) -> a -> b -> c -> Svg
lazy3 =
    VirtualDom.lazy3
