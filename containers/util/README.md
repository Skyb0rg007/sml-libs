# Internal utility module

This directory holds all definitions that rely on compile-time options,
choice of compiler, or other abstractable definitions.

- structure Either
  * MLton doesn't have a generic sum type, so include a definition of
    the `Either` structure from the SML updated basis
  * For SML/NJ, just re-export the structure

- structure VectorEx
  * MLton vector creation primitives

- structure WordEx
  * Bitwise primitives whose optimal definition depends on word size

- structure Util
  * Miscellaneous utilities

- structure Weak
  * Weak references
