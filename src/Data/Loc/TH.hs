{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Derive shallow source-location instances for algebraic datatypes.
--
-- Each constructor may have zero or one directly declared 'Loc.SrcLoc' field.
-- Type synonyms for that field are expanded. Fields containing locations, such
-- as lists, child nodes, and type variables, are payloads and are not traversed.
-- A type-variable field remains a payload even when instantiated to
-- 'Loc.SrcLoc'. Type families are not reduced.
--
-- The generated methods do not require instances for payload types. They inspect
-- the outer constructor without forcing lazy payload fields. Relocation
-- replaces only the constructor's own location. Constructors without a location
-- return 'Loc.NoLoc' and remain unchanged, without evaluating the replacement
-- location. Datatype field strictness still applies when rebuilding a node.
--
-- Put the splice after the datatype declarations and before declarations that
-- use the derived instances. The derivation functions must be imported from
-- this module because of Template Haskell's stage restriction.
--
-- > data Node = Node String SrcLoc | Empty
-- > $(deriveLocatedAndRelocatable ''Node)
--
-- Ordinary constructors, records, infix constructors, existential constructors,
-- and GADT constructors are supported. Empty datatypes are supported with
-- constant location lookup and identity relocation. Derivation fails for
-- constructors with multiple direct locations, datatype contexts, and targets
-- other than ordinary datatypes or newtypes, including data families.
module Data.Loc.TH
    ( deriveLocated
    , deriveRelocatable
    , deriveLocatedAndRelocatable
    ) where

import           Control.Monad       (forM, replicateM)
import           Data.List           (elemIndices)
import           Language.Haskell.TH hiding (location)

import qualified Data.Loc            as Loc

-- | Derive 'Loc.Located' using the constructor's direct 'Loc.SrcLoc' field.
-- Constructors without that field have 'Loc.NoLoc'. See the module contract
-- for supported declarations and compile-time failures.
deriveLocated :: Name -> Q [Dec]
deriveLocated name = do
    (typ, constructors) <- inspectDatatype name
    dec <- locatedInstance typ constructors
    return [dec]

-- | Derive shallow 'Loc.Relocatable'. Payloads and child locations are
-- preserved. See the module contract for supported declarations and
-- compile-time failures.
deriveRelocatable :: Name -> Q [Dec]
deriveRelocatable name = do
    (typ, constructors) <- inspectDatatype name
    dec <- relocatableInstance typ constructors
    return [dec]

-- | Derive both 'Loc.Located' and 'Loc.Relocatable' with the same field policy.
deriveLocatedAndRelocatable :: Name -> Q [Dec]
deriveLocatedAndRelocatable name = do
    (typ, constructors) <- inspectDatatype name
    located <- locatedInstance typ constructors
    relocatable <- relocatableInstance typ constructors
    return [located, relocatable]

-- The index identifies the only direct location, if present.
data Constructor = Constructor Name Int (Maybe Int)

inspectDatatype :: Name -> Q (Type, [Constructor])
inspectDatatype name = do
    info <- reify name
    case info of
      TyConI (DataD context _ variables _ constructors _) ->
          inspect context (visibleVariables variables) constructors
      TyConI (NewtypeD context _ variables _ constructor _) ->
          inspect context (visibleVariables variables) [constructor]
      _ -> fail $ "Data.Loc.TH: expected a datatype or newtype: " ++ show name
  where
    inspect context variables constructors
        | null context = do
            cs <- concat <$> mapM inspectConstructor constructors
            return (foldl AppT (ConT name) (map VarT variables), cs)
        | otherwise    = fail $ "Data.Loc.TH: datatype contexts are unsupported: " ++ show name

#if MIN_VERSION_template_haskell(2,21,0)
visibleVariables :: [TyVarBndr BndrVis] -> [Name]
visibleVariables variables = [variableName v | v <- variables, visible v]
  where
    visible (PlainTV _ BndrReq)    = True
    visible (KindedTV _ BndrReq _) = True
    visible _                      = False
#elif MIN_VERSION_template_haskell(2,17,0)
visibleVariables :: [TyVarBndr ()] -> [Name]
visibleVariables = map variableName
#else
visibleVariables :: [TyVarBndr] -> [Name]
visibleVariables = map variableName
#endif

#if MIN_VERSION_template_haskell(2,17,0)
variableName :: TyVarBndr flag -> Name
variableName (PlainTV name _)    = name
variableName (KindedTV name _ _) = name
#else
variableName :: TyVarBndr -> Name
variableName (PlainTV name)    = name
variableName (KindedTV name _) = name
#endif

inspectConstructor :: Con -> Q [Constructor]
inspectConstructor (NormalC name fields)       = inspectFields [name] (map snd fields)
inspectConstructor (RecC name fields)          = inspectFields [name] [typ | (_, _, typ) <- fields]
inspectConstructor (InfixC left name right)    = inspectFields [name] [snd left, snd right]
inspectConstructor (ForallC _ _ constructor)   = inspectConstructor constructor
inspectConstructor (GadtC names fields _)      = inspectFields names (map snd fields)
inspectConstructor (RecGadtC names fields _)   = inspectFields names [typ | (_, _, typ) <- fields]

inspectFields :: [Name] -> [Type] -> Q [Constructor]
inspectFields names fields = do
    locations <- elemIndices True <$> mapM isSourceLocation fields
    forM names $ \name ->
        case locations of
          []      -> return $ Constructor name (length fields) Nothing
          [index] -> return $ Constructor name (length fields) (Just index)
          _       -> fail $ "Data.Loc.TH: multiple SrcLoc fields in constructor " ++ show name

-- Expand only the outer type synonym. Nested locations remain payloads.
isSourceLocation :: Type -> Q Bool
isSourceLocation typ =
    case splitType typ of
      (ConT name, []) | name == ''Loc.SrcLoc -> return True
      (ConT name, arguments) -> do
          info <- reify name
          case info of
            TyConI (TySynD _ variables body) ->
                let names = visibleVariables variables
                in if length arguments < length names
                   then return False
                   else isSourceLocation $
                       foldl AppT (substitute (zip names arguments) body)
                                  (drop (length names) arguments)
            _ -> return False
      _ -> return False

splitType :: Type -> (Type, [Type])
splitType (AppT typ argument) = let (headType, arguments) = splitType typ
                               in (headType, arguments ++ [argument])
splitType (SigT typ _)        = splitType typ
splitType (ParensT typ)       = splitType typ
splitType (InfixT a name b)   = splitType (AppT (AppT (ConT name) a) b)
splitType (UInfixT a name b)  = splitType (AppT (AppT (ConT name) a) b)
#if MIN_VERSION_template_haskell(2,15,0)
splitType (AppKindT typ _)    = splitType typ
#endif
splitType typ                = (typ, [])

substitute :: [(Name, Type)] -> Type -> Type
substitute bindings (VarT name)        = maybe (VarT name) id (lookup name bindings)
substitute bindings (AppT typ arg)     = AppT (substitute bindings typ) (substitute bindings arg)
substitute bindings (SigT typ kind)    = SigT (substitute bindings typ) kind
substitute bindings (ParensT typ)      = ParensT (substitute bindings typ)
substitute bindings (InfixT a name b)  = InfixT (substitute bindings a) name (substitute bindings b)
substitute bindings (UInfixT a name b) = UInfixT (substitute bindings a) name (substitute bindings b)
#if MIN_VERSION_template_haskell(2,15,0)
substitute bindings (AppKindT typ k)   = AppKindT (substitute bindings typ) k
#endif
substitute _        typ                = typ

locatedInstance :: Type -> [Constructor] -> Q Dec
locatedInstance typ constructors =
    instanceD (cxt []) (appT (conT ''Loc.Located) (return typ))
        [funD 'Loc.locOf clauses]
  where
    clauses
        | null constructors = [clause [wildP] (normalB [| Loc.NoLoc |]) []]
        | otherwise         = map locatedClause constructors

locatedClause :: Constructor -> Q Clause
locatedClause (Constructor name arity location) = do
    loc <- newName "location"
    let fields = [if Just index == location then varP loc else wildP | index <- [0 .. arity - 1]]
        result = case location of
                   Nothing -> [| Loc.NoLoc |]
                   Just _  -> [| Loc.locOf $(varE loc) |]
    clause [conP name fields] (normalB result) []

relocatableInstance :: Type -> [Constructor] -> Q Dec
relocatableInstance typ constructors =
    instanceD (cxt []) (appT (conT ''Loc.Relocatable) (return typ))
        [funD 'Loc.reloc clauses]
  where
    clauses
        | null constructors = [clause [wildP, varP node] (normalB (varE node)) []]
        | otherwise         = map relocatableClause constructors
    node = mkName "node"

relocatableClause :: Constructor -> Q Clause
relocatableClause (Constructor name arity Nothing)         = do
    node <- newName "node"
    clause [wildP, asP node (conP name (replicate arity wildP))]
           (normalB (varE node)) []
relocatableClause (Constructor name arity (Just location)) = do
    loc <- newName "location"
    fields <- replicateM arity (newName "field")
    let patterns = [if index == location then wildP else varP field | (index, field) <- zip [0..] fields]
        values = [if index == location then [| Loc.fromLoc $(varE loc) |] else varE field
                 | (index, field) <- zip [0..] fields]
    clause [varP loc, conP name patterns]
           (normalB (foldl appE (conE name) values)) []
