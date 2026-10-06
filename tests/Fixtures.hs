{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Fixtures
    ( Node(..)
    , Record(..)
    , Location(..)
    , Payload(..)
    , Infix(..)
    , Strict(..)
    , Some(..)
    , Indexed(..)
    , RecordGadt(..)
    , Phantom(..)
    , Aliased(..)
    , Polymorphic(..)
    , FamilyPayload(..)
    , Family(..)
    , Empty
    , LocatedOnly(..)
    , RelocatableOnly(..)
    , Ambiguous(..)
    , AmbiguousAlias(..)
    , rejectAmbiguousLocated
    , rejectAmbiguousRelocatable
    , rejectAmbiguousAlias
    , rejectSynonymTarget
    , rejectClassTarget
    , rejectDataFamilyTarget
    ) where

import           Data.Loc
import           Data.Loc.TH
import           Language.Haskell.TH (recover)

data Node
    = Leaf (Int -> Int) SrcLoc
    | Branch Node SrcLoc Node
    | Wrapper Node
    | Plain (Maybe SrcLoc)

data Record a = Record
    { recordLocation :: SrcLoc
    , recordValue    :: a
    }

newtype Location = Location SrcLoc

newtype Payload a = Payload a

data Infix = SrcLoc :@: Int

data Strict = Strict Int !SrcLoc

data Some = forall a. Show a => Some a SrcLoc

data Indexed a where
    Indexed :: a -> SrcLoc -> Indexed a
    Refined :: SrcLoc -> Indexed Int
    First, Second :: SrcLoc -> Indexed Bool

data RecordGadt a where
    RecordGadt :: { gadtValue :: a, gadtLocation :: SrcLoc } -> RecordGadt a

data Phantom (a :: k) = Phantom SrcLoc

type Identity a = a
type LocationAlias = Identity (Identity SrcLoc)

data Aliased = Aliased LocationAlias

data Polymorphic a = Polymorphic a

type family LocationFamily a where
    LocationFamily Int = SrcLoc

data FamilyPayload = FamilyPayload (LocationFamily Int)

data family Family a

data instance Family Int = FamilyInt SrcLoc

data Empty

data LocatedOnly = LocatedOnly SrcLoc

data RelocatableOnly = RelocatableOnly Int SrcLoc

data Ambiguous = Ambiguous SrcLoc SrcLoc

data AmbiguousAlias = AmbiguousAlias LocationAlias SrcLoc

$(concat <$> mapM deriveLocatedAndRelocatable
    [''Node, ''Record, ''Location, ''Payload, ''Infix, ''Strict, ''Some,
     ''Indexed, ''RecordGadt, ''Phantom, ''Aliased, ''Polymorphic,
     ''FamilyPayload, ''Empty])

$(deriveLocated ''LocatedOnly)

$(deriveRelocatable ''RelocatableOnly)

rejectAmbiguousLocated :: Bool
rejectAmbiguousLocated = $(recover [| True |] (deriveLocated ''Ambiguous >> [| False |]))

rejectAmbiguousRelocatable :: Bool
rejectAmbiguousRelocatable = $(recover [| True |] (deriveRelocatable ''Ambiguous >> [| False |]))

rejectAmbiguousAlias :: Bool
rejectAmbiguousAlias = $(recover [| True |] (deriveLocatedAndRelocatable ''AmbiguousAlias >> [| False |]))

rejectSynonymTarget :: Bool
rejectSynonymTarget = $(recover [| True |] (deriveLocated ''LocationAlias >> [| False |]))

rejectClassTarget :: Bool
rejectClassTarget = $(recover [| True |] (deriveRelocatable ''Located >> [| False |]))

rejectDataFamilyTarget :: Bool
rejectDataFamilyTarget = $(recover [| True |] (deriveLocated ''Family >> [| False |]))
