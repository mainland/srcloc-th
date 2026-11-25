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
supported declarations, laziness guarantees, and compile-time failures. The
package supports GHC 8.0 through 9.14.

Build and test with Cabal:

```sh
cabal build all
cabal test all --test-show-details=direct
cabal haddock all
```

## AI assistance

Version `1.0.0` is this package's first release. AI tools were used in its
development, review, and release preparation. The maintainer remains
responsible for this release.
