# srcloc-th

`srcloc-th` derives shallow `Located` and `Relocatable` instances for datatypes
with direct `SrcLoc` fields. It keeps Template Haskell out of the runtime
`srcloc` package and generates ordinary constructor matches without payload
constraints.

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.Loc
import Data.Loc.TH

data Node = Node String SrcLoc | Empty

$(deriveLocatedAndRelocatable ''Node)
```

See the `Data.Loc.TH` Haddock documentation for the field-selection policy,
supported declarations, laziness guarantees, and compile-time failures.
The package supports GHC 8.0 and later.

Build and test with Cabal:

```sh
cabal build all
cabal test all --test-show-details=direct
cabal haddock all
```
