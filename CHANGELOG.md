# Changelog

## 1.0.0

- Add `Data.Loc.TH` with separate and combined derivation of shallow `Located`
  and `Relocatable` instances.
- Recognize direct `SrcLoc` fields and their type synonyms. Reject constructors
  with multiple direct locations at compile time.
- Support GHC 8.0 through 9.14.
- Mark the package as stable.
