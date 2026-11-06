{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w -Wname-shadowing -Werror #-}

-- | Derive instances for an empty datatype in a module that defines a
-- top-level @node@. The generated binders must not shadow user names, which
-- @-Wname-shadowing@ and @-Werror@ would reject. Every other warning is off in
-- this module, so a warning that a later compiler adds cannot fail its build.
module Shadowing
    ( Void
    , node
    ) where

import           Data.Loc.TH

data Void

node :: ()
node = ()

$(deriveLocatedAndRelocatable ''Void)
