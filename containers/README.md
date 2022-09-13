
# Container library for Standard ML

This library is a collection of useful data structures

Goals:

- Full SML/NJ and MLton compatibility
  * This includes compiler-specific idioms
    - Ex. Using `MLton.Vector.create`; avoiding `Word32` and `Word64`
  * Should compile with any MLton option (Specifically `default-type` options)
  * No SML/NJ warnings should escape
    - Ok if silenced in .cm file
  * Avoid SML/NJ library name clashes
    - Ex. use `HASHABLE_TYPE` instead of `HASH_KEY`

- Complete and unified data structure interfaces
  * Avoid SML/NJ library problem where you cannot satisfy the `ORD_MAP` signature with a hash-based map
  * Each data structure implementation satisfies the unified interface
    - Implementation-specific functions are still exposed, just not required by others
  * More functions in each interface
    - No implementation has to sacrifice speed for a given function
    - Ex. `mapAccumL`
  * No exceptions or continuations
    - Exception handling is pretty expensive in SML/NJ and continuation
      capture is very expensive for MLton
    - Encourage using `Option.valOf` to indicate that the function can throw
  * Use SML/NJ terminology when the name doesn't violate an above rule
    - Use `find` over `lookup` for an `option`-returning lookup,
      even though Haskell and OCaml both disagree
    - Change `delete` to return an `option`; the SML/NJ function throws

- Embrace mutable implementations of functional data structures
  * Designed for speed, not elegance
  * Downside: continuation capture inside library functions likely doesn't
    behave consistently

What's included:

- Persistent/Functional Data Structures
  * Maps
    - [Hash array mapped tries](https://en.wikipedia.org/wiki/Hash_array_mapped_trie)
    - Big-endian [Patricia trees](https://en.wikipedia.org/wiki/Radix_tree)
      * [Augmented](https://en.wikipedia.org/wiki/Augmented_map) version
      * Lazy structure sharing version
      * [Hash-cons](https://en.wikipedia.org/wiki/Hash_consing)ed version (TODO)
  * Lazy Sequences

- Mutable Data Structures
  * [Disjoint-Set/Union-Find](https://en.wikipedia.org/wiki/Disjoint-set_data_structure)
  * Tables (Maps)
    - Hash table with separate chaining
      * Unfortunately probing is not too efficient without allowing for uninitialized memory
      * Weak-value version (TODO)

References:

- The [containers](https://hackage.haskell.org/package/containers) package (Haskell committee; many contributors)
- The [unordered-containers](https://hackage.haskell.org/package/unordered-containers) package (Johan Tibell and others)
- The [internal MLton library](https://github.com/MLton/mlton/tree/master/lib/mlton)
- The [OCaml Stdlib library](https://github.com/ocaml/ocaml/tree/trunk/stdlib)
- [Incremental Computation and the Incremental Evaulation of Functional Programs](https://ecommons.cornell.edu/handle/1813/6776) by William Pugh

