newtype-generics
================

A typeclass and set of functions for working with newtypes.
Fork of the code published by Darius Jahandarie [here](http://hackage.haskell.org/package/newtype-0.2),
with the addition of generics.

The 'Newtype' typeclass and related functions: `op`, `ala`, `ala'`, `under`. 
Primarly pulled from Conor McBride's Epigram work. Some examples:

```
-- foldMaps the list ala the Sum newtype. This results in 10.
ala Sum foldMap [1,2,3,4] 

-- foldMaps the list ala the Product newtype. This results in 24.
ala Product foldMap [1,2,3,4] 

-- foldMaps the list ala the Endo newtype. This results in 8.
ala Endo foldMap [(+1), (+2), (subtract 1), (*2)] 3 
```

_NB:_ `Data.Foldable.foldMap` is a generalized `mconcatMap` which is a generalized `concatMap`.

This package includes `Newtype` instances for all the (non-GHC/foreign) newtypes in base (as seen in the examples).
However, there are neat things you can do with this with /any/ newtype and you should definitely define your own 'Newtype' instances for the power of this library.
For example, see `ala Cont traverse`, with the proper `Newtype` instance for Cont.

This could of course be eased with the addition of generics for version 0.3:

```
{-# LANGUAGE DeriveGeneric              #-}

import GHC.Generics
(...)
newtype Example = Example Int (deriving Generic)
instance Newtype Example
```

