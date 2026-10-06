{-# LANGUAGE GADTs #-}

module Main (main) where

import           Control.Exception (ErrorCall, displayException, evaluate, try)
import           Data.List         (isInfixOf)
import           Data.Loc
import           Fixtures
import           Test.Tasty        (defaultMain, testGroup)
import           Test.Tasty.HUnit  (assertBool, testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "TH locations"
    [ testCase "lookup does not force function payload" $
        locOf (Leaf undefined original) @?= originalLoc
    , testCase "relocation preserves function payload" $
        case reloc replacementLoc (Leaf (+ 1) original) of
          Leaf f l -> (f 41, locOf l) @?= (42, replacementLoc)
          _        -> fail "constructor changed"
    , testCase "middle field selects own location" $
        locOf (Branch undefined original undefined) @?= originalLoc
    , testCase "relocation is shallow" $
        case reloc replacementLoc (Branch (Leaf id original) original (Leaf id original)) of
          Branch left l right -> (locOf left, locOf l, locOf right)
                                 @?= (originalLoc, replacementLoc, originalLoc)
          _                   -> fail "constructor changed"
    , testCase "relocation does not force lazy children" $
        locOf (reloc replacementLoc (Branch undefined original undefined)) @?= replacementLoc
    , testCase "relocation does not force old location" $
        locOf (reloc replacementLoc (Leaf undefined undefined)) @?= replacementLoc
    , testCase "relocation does not force a lazy replacement" $
        case reloc undefined (Leaf (+ 1) original) of
          Leaf f _ -> f 41 @?= 42
          _        -> fail "constructor changed"
    , testCase "wrapper has no direct location" $
        locOf (Wrapper undefined) @?= NoLoc
    , testCase "wrapper preserves child and ignores replacement" $
        case reloc undefined (Wrapper (Leaf id original)) of
          Wrapper child -> locOf child @?= originalLoc
          _             -> fail "constructor changed"
    , testCase "nested SrcLoc is a payload" $
        locOf (Plain undefined) @?= NoLoc
    , testCase "nested SrcLoc remains unchanged" $
        case reloc replacementLoc (Plain (Just original)) of
          Plain (Just l) -> locOf l @?= originalLoc
          _              -> fail "payload changed"
    , testCase "record lookup ignores payload" $
        locOf (Record original (undefined :: Int)) @?= originalLoc
    , testCase "record relocation preserves payload" $
        let node = reloc replacementLoc (Record original (42 :: Int))
        in (recordValue node, locOf node) @?= (42, replacementLoc)
    , testCase "newtype location lookup" $
        locOf (Location original) @?= originalLoc
    , testCase "newtype location relocation" $
        locOf (reloc replacementLoc (Location original)) @?= replacementLoc
    , testCase "newtype payload lookup stays lazy" $
        locOf (Payload (undefined :: Int)) @?= NoLoc
    , testCase "newtype payload relocation stays lazy" $
        locOf (reloc undefined (Payload (undefined :: Int))) @?= NoLoc
    , testCase "infix location lookup" $
        locOf (original :@: undefined) @?= originalLoc
    , testCase "infix relocation preserves payload" $
        case reloc replacementLoc (original :@: 42) of
          l :@: value -> (locOf l, value) @?= (replacementLoc, 42)
    , testCase "strict location relocation keeps lazy payload" $
        locOf (reloc replacementLoc (Strict undefined original)) @?= replacementLoc
    , testCase "relocation respects a strict location field" $ do
        result <- try (evaluate (reloc (error "replacement forced") (Strict 42 original)))
                  :: IO (Either ErrorCall Strict)
        case result of
          Left err -> assertBool "unexpected exception" ("replacement forced" `isInfixOf` displayException err)
          Right _  -> fail "strict location was not evaluated"
    , testCase "existential relocation preserves its dictionary" $
        case reloc replacementLoc (Some (42 :: Int) original) of
          Some value l -> (show value, locOf l) @?= ("42", replacementLoc)
    , testCase "GADT relocation preserves payload" $
        case reloc replacementLoc (Indexed (42 :: Int) original) of
          Indexed value l -> (value, locOf l) @?= (42, replacementLoc)
          _               -> fail "constructor changed"
    , testCase "refined GADT location lookup" $
        locOf (Refined original) @?= originalLoc
    , testCase "shared GADT signature covers both constructors" $
        (locOf (reloc replacementLoc (First original)), locOf (Second original))
        @?= (replacementLoc, originalLoc)
    , testCase "record GADT location relocation" $
        let node = reloc replacementLoc (RecordGadt (42 :: Int) original)
        in (gadtValue node, locOf node) @?= (42, replacementLoc)
    , testCase "polykinded parameter is not a payload constraint" $
        locOf (reloc replacementLoc (Phantom original :: Phantom Maybe)) @?= replacementLoc
    , testCase "parameterized nested aliases select SrcLoc" $
        locOf (reloc replacementLoc (Aliased original)) @?= replacementLoc
    , testCase "polymorphic SrcLoc payload is not selected" $
        locOf (Polymorphic original) @?= NoLoc
    , testCase "polymorphic SrcLoc payload is preserved" $
        case reloc undefined (Polymorphic original) of
          Polymorphic l -> locOf l @?= originalLoc
    , testCase "type families are not reduced" $
        locOf (FamilyPayload undefined) @?= NoLoc
    , testCase "type-family payload is preserved" $
        case reloc undefined (FamilyPayload original) of
          FamilyPayload l -> locOf l @?= originalLoc
    , testCase "empty datatype location is constant" $
        locOf (undefined :: Empty) @?= NoLoc
    , testCase "empty datatype relocation is identity" $
        locOf (reloc undefined (undefined :: Empty)) @?= NoLoc
    , testCase "separate Located derivation" $
        locOf (LocatedOnly original) @?= originalLoc
    , testCase "separate Relocatable derivation" $
        case reloc replacementLoc (RelocatableOnly 42 original) of
          RelocatableOnly value l -> (value, locOf l) @?= (42, replacementLoc)
    , testCase "ambiguous Located instance rejected" $
        assertBool "derivation succeeded" rejectAmbiguousLocated
    , testCase "ambiguous Relocatable instance rejected" $
        assertBool "derivation succeeded" rejectAmbiguousRelocatable
    , testCase "ambiguous aliases rejected" $
        assertBool "derivation succeeded" rejectAmbiguousAlias
    , testCase "synonym target rejected" $
        assertBool "derivation succeeded" rejectSynonymTarget
    , testCase "class target rejected" $
        assertBool "derivation succeeded" rejectClassTarget
    , testCase "data-family target rejected" $
        assertBool "derivation succeeded" rejectDataFamilyTarget
    ]
  where
    originalLoc    = locOf (startPos "original.hs")
    replacementLoc = locOf (startPos "replacement.hs")
    original       = fromLoc originalLoc
