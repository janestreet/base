## Release v0.17.0

Added functionality:
* Add `String.to_sequence`
* Derive `equal` on `Set.Merge_with_duplicates_element.t`
* Add `Queue.drain`
* Add `Or_error.of_option`
* Extend `Hashtbl_intf.Hashtbl` with `capacity`, intended for testing resizing behavior
* Add `Nothing.must_be_*` functions discarding (parts of) inputs with `Nothing.t` as a
  type parameter
* Add `Float.log2`
* Add `Random.bits64`, re-exported from `Stdlib.Random`
* Add `String.edit_distance` to compute Levenshtein distance between strings
* Add `Comparator.to_module` and `Comparator.of_module`, converting between `Comparator.t` and `Comparator.Module.t`
* Add `Map.sum`, `unzip`, `of_list_with_key_fold`, and `of_list_with_key_reduce`
* Add `Applicative.Ident`, similar to `Monad.Ident`
* Extend `Uniform_array` with more operations akin to `Array`
* Added `List.singleton`
* Add `Map.merge_disjoint_exn` for merging two disjoint maps of the same key/value types
  Raises an exception if there are conflicting keys
* Added `Sequence.Expert.View` to consume sequences more flexibly and efficiently
* Add a `Binary` submodule to `Int`, `Int32`, etc, which provide `to_string` and
  `sexp_of_t` with syntax matching the ocaml binary int literal syntax
* Add `List.stable_dedup` and deprecate `Set.stable_dedup_list`
* Add `Queue.enqueue_front` and `Queue.dequeue_back`
* Add `Type_equal.Id.Create*` functors for polymorphic types

Added unicode support:
* Added `Utf*` submodules to `Bytes`, `Uchar`, and `String`
* Types for `Uchar` and `String` encoding UTF-8, UTF-16LE, UTF-16BE, UTF-32LE, UTF32-BE
* Added conversions, read/write functions, etc

Changed behavior:
* `Info` has improved parsing of DOS newlines and trailing newlines in backtraces

Removed definitions that were previously deprecated:
* `Type_equal.equal` type alias now that destructive update is available
* `Map.comparator` and `Set.comparator` type aliases
* `Base.Popcount`, as it is exported via the various `Int*` modules
* `Option` functions from `Container` that are not useful
* `Result.ok_fst`, an alias for `to_either`
* `Sequence.merge`, an alias for `merge_deduped_and_sorted`
* `Info.to_string_hum_deprecated`
* `?trunc_after` flag to `Info.of_list`, no longer used

Deprecated:
* `Type_equal.Injective`, now that injectivity annotations exist

Removed without deprecating:
* `Type_equal.Id.Uid.to_string`, `of_string`, and `t_of_sexp`. These were not compatible
  with the representation changes needed for the new `Id.Create*` functors

Interface changes:
* `Container.Generic` now supports two "extra" (non-element) type parameters
* Abstracted some of `Map` into `Dictionary_immutable` interfaces
* Abstracted some of `Hashtbl` into `Dictionary_mutable` interfaces
* Export `Set.Poly.set` type rather than using destructive update

Bug fixes:
* Indexing was wrong in `Sequence.findi`, now fixed and with a regression test

Performance improvements:
* Split up and refactored tests in `Base` to reduce build times
* Stop using exceptions for control, primarily to speed up `js_of_ocaml` versions. Affects
  `Sequence.compare`, `String.index`, `String.rindex`, `String.index_from`,
  `String.rindex_from`, `Map.change`, and `Map.remove`
* Unboxed `Int64.pow`
* Branchless implementation of `Float.clamp_unchecked`, `Int.clamp_unchecked`
* Branchless loop body in `Array.count` and `Array.counti`
* Remove allocation in `List.Assoc.find_exn`
* Reduce allocation of `With_return` under some compiler configurations
* Restore `Array.equal` to zero allocation
* Avoid boxing in `Int64.to_int_exn`, `Int64.hash_fold_t`, `Float.hash_fold_t`
* Inlining annotation on `Float.sign_exn` to avoid boxing
* Add `[@cold]` annotation to `Error.raise`
* Tighten up conditional logic in `Hashtbl.set` and `Hashtbl.remove`
* Reducing redundant computation in various `Map` functions
* Rewrite `Array.min_elt` and `Array.max_elt` to reduce branching and allocation
* Improve `ppx_hash` derived code for enumeration-like variants.
* Moved queue mutation check to a function marked `[@cold]`.
* Rewrite `List.dedup_and_sort` without a final remove-duplicates pass.
* Fix `Info` to avoid blowing up the stack on `force` of the internal `lazy`.
* Make `List.take`, `List.drop`, and `List.split` return the original list when possible.
  (issue 153, thanks `@mroch`)
* Adapted `List` functions to take advantage of `[@tail_mod_cons]` where beneficial.
* Use `[@tail_mod_cons]` in `Sequence.to_list`

Refactoring:
* Update whitespace styling, primarily by no longer using ocp-indent on code
* Use `Stdlib.Sys.Immediate64` in `Int63` instead of hand-written copy
* Properly use loop variable in `List.chunks_of` helper
* Rename internal variable in `Hashtbl.remove` for clarity
* Lift some of `Int_conversions` to `Int_string_conversions` to share elsewhere
* Remove unused `[@tailcall]` attribute in `List.group`
* Use inlined records in `Map` and `Set` internal variant representations
* Reimplement `Map.Build_increasing` to something simpler
* Remove unnecessary helper in `Map.remove`
* Remove unused functions from `String0`
* Use inlining instead of duplication for helpers in `Set` implementation
* Split out `Ocaml_intrinsics_kernel`, used it for some intrinsics in `Base`

Documentation:
* Fix typo where `Set.union_list` documented itself as `union`
* Various grammar and capitalization fixes (PR 145, thanks `@goodship1`)

Tests and benchmarks, largely to gain confidence in the changes above:
* Updated allocation expect tests to actually use `let%expect_test` (oops)
* Updated benchmarks for `Float.clamp*`.
* Add benchmarks for `Hashtbl.map_inplace`, `create`, `remove`, `set`, `change`, and
  `find_and_remove`
* Add benchmarks for `Map.set`, `remove`, and `change`
* Add `js_of_ocaml`-only benchmarks for `Map.remove` and `Map.change`
* Benchmark `Sequence.compare` and `String.index`
* Benchmark `Set.add`, `find`, and `find_map`
* Tests and benchmarks for `min_elt`, `max_elt`, `count`, and `counti` in `Array` and
  `List`

Windows:
* Fixed the windows build. (PR 152, thanks `@hhugo`)

Work toward compatibility with OCaml 5.1:
* Update `Random` to use new splittable PRNG
* Other various changes

Improved support for compiler extensions found at https://github.com/ocaml-flambda/flambda-backend:
* Various updated signatures, definitions, and new functionality to support `local_` mode
  and stack allocation
* Added annotations for `[@zero_alloc]` compiler checks

## Release v0.16.0

Changes across many modules:

* Replaced `Caml` with `Stdlib`. The `Caml` module predated `Stdlib` and has been
  redundant for some time.

* Added support for local allocations. This is a nonstandard OCaml extension available at
  <https://github.com/ocaml-flambda/ocaml-jst>.

  Support includes:
  - updating functions to accept `[@local]` arguments, especially closures
  - local constructors, like `Array.create_local` and `Bytes.create_local`
  - new versions of interfaces supporting `local` values, such as `Applicative.S_local`
  - `[@@deriving globalize]` on some types, for converting local values to global values

* Rename `Polymorphic_compare` submodules to `Comparisons`. The former was a misnomer.
  While the comparisons for a given type are meant to replace polymorphic compare
  operators, they are not polymorphic themselves.

* Added `Container.S_with_creators` and `Indexed_container.S_with_creators`. Used these in
  container modules such as `Array`, `List`, and `String`. These interfaces standardize
  functions like `map` and `filter`. Along the way, refactored module types in `Container`
  and `Indexed_container`.

* In signatures for `fold*` functions, renamed accumulator type variables to `'acc` for
  improved readability.

* Added `of_string_opt` to `Int_intf.S`.

* Added `dequeue_and_ignore_exn` to `Queue_intf.S`.

Changes to individual modules:

* `Bool`: added `select`, a primitive using `CMOV` on architectures that support it.

* `Comparable`:
  * Added `'a reversed` and `compare_reversed`, to support deriving inverted comparisons,
    e.g.: `[%compare: My_type.t Comparable.reversed]`
  * Added `Derived2_phantom`, similar to `Derived_phantom`.
  * Made `Derived*.comparator_witness` types injective.

* `Float`:
  * Added hyperbolic trig functions `acosh`, `asinh`, and `atanh` to `Float`.
  * Added `Float.of_string_opt`.

* `Hash_set`: Made `t` injective.

* `Hashtbl`:
  * Added `choose_randomly` and `choose_randomly_exn`.
  * Made `Hashtbl.t` injective.

* `Lazy`: Added `peek`, extracting an already-forced value if present.

* `Map`:
  * Added `split_le_gt`, `split_lt_ge`, and `transpose_keys`.
  * Added `Make_applicative_traversals`, allowing some applicatives to improve performance
    when operating on maps.
  * Corrected documentation of performance for `filter*` functions.
  * Refactored module types in `map_intf.ml`. Among other changes, propagated
    `~comparator` argument slightly differently to allow expressing type of
    `transpose_keys` properly.

* `Monad`: Documented performance characteristics of `Ident`.

* `Option`: Deprecated functions from `Container` but not particularly useful for options.

* `Ppx_compare_lib`: Removed primitive functions; `ppx_compare` now explicitly refers to
  these via `Stdlib`.

* `Sequence`: Changed `Step.t` variant type to use inlined records.

* `Set`:
  * Added `of_tree`, `to_tree`, `split_le_gt`, and `split_lt_ge`.
  * Created a single shared `'a Named.t` type to `set_intf.ml`, rather than using a new type
    in every instance of `Accessors`.
  * Made `Set.t` injective in both type arguments.
  * Refactored module types in `set_intf.ml`.

* `Sexpable`: `Of_stringable` now provides `t_sexp_grammar`.

* `Sign` and `Sign_or_nan`: Added `to_string_hum`.

* `Stack`: added `filter`, `filter_inplace`, and `filter_map`.

* `String`: added `concat_lines`, `pad_left`, `pad_right`, and `unsafe_sub`

* `Sys`: added `opaque_identity_global`, which forces its argument to be globally
  allocated.

* `Type_equal`: `Id.Uid` now implements `Identifiable.S`

* `Uniform_array`: add `sort`

## Old pre-v0.15 changelogs (very likely stale and incomplete)

## git version

- Renamed `Result.ok_fst` to `Result.to_either` (old name remains as
  deprecated alias).  Added analogous `Result.of_either` function.

- Removed deprecated values `Array.truncate`, `{Obj_array,
  Uniform_array}.unsafe_truncate`, `Result.ok_unit`, `{Result,
  Or_error}.ignore`.

- Changed the signature of `Hashtbl.equal` to take the data equality
  function first, allowing it to be used with `[%equal: t]`.

- Remove deprecated function `List.dedup`.

- Remove deprecated string mutation functions from the `String` module.

- Removed deprecated function `Monad.all_ignore` in favor of
  `Monad.all_unit`.

- Deprecated `Or_error.ignore` and `Result.ignore` in favor of
  `Or_error.ignore_m` and `Result.ignore_m`.

- `Ordered_collection_common.get_pos_len` now returns an `Or_error.t`

- Added `Bool.Non_short_circuiting`.

- Added `Float.square`.

- Remove module `Or_error.Ok`.

- module `Ref` doesn't implement `Container.S1` anymore.

- Rename parameter of `Sequence.merge` from `cmp` to `compare`.

- Added `Info.of_lazy_t`

- Added `List.partition_result` function, to partition a list of `Result.t`
  values

- Changed the signature of `equal` from `'a t -> 'a t -> equal:('a -> 'a ->
  bool) -> bool` to `('a -> 'a -> bool) -> 'a t -> 'a t -> bool`.

- Optimized `Lazy.compare` to check physical equality before forcing the lazy
  values.

- Deprecated `Args` in the `Applicative` interface in favor of using `ppx_let`.

- Deprecated `Array.replace arr i ~f` in favor of using `arr.(i) <- (f (arr.(i)))`

- Rename collection length parameter of `Ordered_collection_common` functions
  from `length` to `total_length`, and add a unit argument to `get_pos_len` and
  `get_pos_len_exn`.

- Removed functions that were deprecated in 2016 from the `Array` and `Set`
  modules.

- `Int.Hex.of_string` and friends no longer silently ignore a suffix
  of non-hexadecimal garbage.

- Added `?backtrace` argument to `Or_error.of_exn_result`.

- `List.zip` now returns a `List.Or_unequal_lengths.t` instead of an `option`.

- Remove functions from the `Sequence` module that were deprecated in 2015.

- `Container.Make` and `Container.Make0` now require callers to either provide a
  custom `length` function or request that one be derived from `fold`.
  `Container.to_array`'s signature is also changed to accept `length` and `iter`
  instead of `fold`.

- Exposed module `Int_math`.

## v0.11

- Deprecated `Not_found`, people who need it can use `Caml.Not_found`, but its
  use isn't recommended.

- Added the `Sexp.Not_found_s` exception which will replace `Caml.Not_found` as
  the default exception in a future release.

- Document that `Array.find_exn`, `Array.find_map_exn`, and `Array.findi_exn`
  may throw `Caml.Not_found` _or_ `Not_found_s`.

- Document that `Hashtbl.find_exn` may throw `Caml.Not_found` _or_
  `Not_found_s`.

- Document that `List.find_exn`, and `List.find_map_exn` may throw
  `Caml.Not_found` _or_ `Not_found_s`.

- Document that `List.find_exn` may throw `Caml.Not_found` _or_ `Not_found_s`.

- Document that `String.lsplit2_exn`, and `String.rsplit2_exn` may throw
  `Caml.Not_found` _or_ `Not_found_s`.

- Added `Sys.backend_type`.

- Removed unnecessary unit argument from `Hashtbl.create`.

- Removed deprecated operations from `Hashtbl`.

- Removed `Hashable.t` constructors from `Hashtbl` and `Hash_set`, instead
  favoring the first-class module constructors.

- Removed `Container` operations from `Either.First` and `Either.Second`.

- Changed the type of `fold_until` in the `Container` interfaces. Rather than
  returning a `Finished_or_stopped_early.t` (which has also been removed), the
  function now takes a `finish` function that will be applied the result if `f`
  never returned a `Stop _`.

- Removed the `String_dict` module.

- Added a `Queue` module that is backed by an `Option_array` for efficient and
  (non-allocating) implementations of most operations.

- Added a `Poly` submodule to `Map` and `Set` that exposes constructors that
  use polymorphic compare.

- Deprecated `all_ignore` in the `Monad` and `Applicative` interfaces in favor
  of `all_unit`.

- Deprecated `Array.replace_all` in favor of `Array.map_inplace`, which is the
  standard name for that sort of operation within Base.

- Document that `List.find_exn`, and `List.find_map_exn` may throw
  `Caml.Not_found` _or_ `Not_found_s`.

- Make `~compare` a required argument to `List.dedup_and_sort`, `List.dedup`,
  `List.find_a_dup`, `List.contains_dup`, and `List.find_all_dups`.

- Removed `List.exn_if_dup`. It is still available in core_kernel.

- Removed "normalized" index operation `List.slice`. It is still available in
  core_kernel.

- Remove "normalized" index operations from `Array`, which incluced
  `Array.normalize`, `Array.slice`, `Array.nget` and `Array.nset`. These
  operations are still available in core_kernel.

- Added `Uniform_array` module that is just like an `Array` except guarantees
  that the representation array is not tagged with `Double_array_tag`, the tag
  for float arrays.

- Added `Option_array` module that allows for a compact representation of `'a
  optoin array`, which avoids allocating heap objects representing `Some a`.

- Remove "normalized" index operations from `String`, which incluced
  `String.normalize`, `String.slice`, `String.nget` and `String.nset`. These
  operations are still available in core_kernel.

- Added missing conversions between `Int63` and other integer types,
  specifically, the versions that return options.

- Added truncating versions of integer conversions, with a suffix of
  `_trunc`.  These allow fast conversions via bit arithmetic without
  any conditional failure; excess bits beyond the width of the output
  type are simply dropped.

- Added `Sequence.group`, similar to `List.group`.

- Reimplemented `String.Caseless.compare` so that it does not
  allocate.

- Added `String.is_substring_at string ~pos ~substring`.  Used it as
  back-end for `is_suffix` and `is_prefix`.

- Moved all remaining `Replace_polymorphic_compare` submodules from Base
  types and consolidated them in one place within `Import0`.

- Removed `(<=.)` and its friends.

- Added `Sys.argv`.

- Added a infix exponentation operator for int.

- Added a `Formatter` module to reexport the `Format.formatter` type and updated
  the deprecation message for `Format`.

## v0.10

(Changes that can break existing programs are marked with a "\*")

### Bugfixes

- Generalized the type of `Printf.ifprintf` to reflect OCaml's stdlib.

- Made `Sequence.fold_m` and `iter_m` respect `Skip` steps and explicitly bind
  when they occur.

- Changed `Float.is_negative` and `is_non_positive` on `NaN` to return `false`
  rather than `true`.

- Fixed the `Validate.protect` function, which was mistakenly raising exceptions.

### API changes

- Renamed `Map.add` as `set`, and deprecated `add`. A later feature will add
  `add` and `add_exn` in the style of `Hashtbl`.

- A different hash function is used to implement `Base.Int.hash`.
  The old implementation was `Int.abs` but collision resistance is not enough,
  we want avalanching as well.
  The new function is an adaptation of one of the
  [Thomas Wang](http://web.archive.org/web/20071223173210/http://www.concentric.net/~Ttwang/tech/inthash.htm)
  hash functions to OCaml (63-bit integers), which results in reasonably good avalanching.


- Made `open Base` expose infix float operators (+., -., etc.).

* Renamed `List.dedup` to `List.dedup_and_sort`, to better reflect its existing behavior.

- Added `Hashtbl.find_multi` and `Map.find_multi`.

- Added function `Map.of_increasing_sequence` for constructing a `Map.t` from an
  ordered `Sequence.t`

- Added function `List.chunks_of : 'a t -> length : int -> 'a t t`, for breaking
  a list into chunks of equal length.

- Add to module `Random` numeric functions that take upper and lower inclusive
  bounds, e.g. `Random.int_incl : int -> int -> int`.

* Replaced `Exn.Never_elide_backtrace` with `Backtrace.elide`, a `ref` cell that
  determines whether `Backtrace.to_string` and `Backtrace.sexp_of_t` elide
  backtraces.

- Exposed infix operator `Base.( @@ )`.

- Exposed modules `Base.Continue_or_stop` and `Finished_or_stopped_early`, used
  with the `Container.fold_until` function.

- Exposed module types Base.T, T1, T2, and T3.

- Added `Sequence.Expert` functions `next_step` and
  `delayed_fold_step`, for clients that want to explicitly handle `Skip` steps.

- Added `Bytes` module.
  This includes the submodules `From_string` and `To_string` with blit
  functions.
  N.B. the signature (and name) of `unsafe_to_string` and `unsafe_of_string` are
  different from the one in the standard library (and hopefully more explicit).

- Add bytes functions to `Buffer`.
  Also added `Buffer.content_bytes`, the analog of `contents` but that returns
  `bytes` rather than `string`.

* Enabled `-safe-string`.

- Added function `Int63.of_int32`, which was missing.

* Deprecated a number of `String` mutating functions.

- Added module `Obj_array`, moved in from `Core_kernel`.

* In module type `Hashtbl.Accessors`, removed deprecated functions, moving them
  into a new module type, `Deprecated`.

- Exported `sexp_*` types that are recognized by `ppx_sexp_*` converters:
  `sexp_array`, `sexp_list`, `sexp_opaque`, `sexp_option`.

* Reworked the `Or_error` module's interface, moving the `Container.S` interface
  to an `Ok` submodule, and adding functions `is_ok`, `is_error`, and `ok` to
  more closely resemble the interface of the `Result` module.

- Removed `Int.O.of_int_exn`.

- Exposed `Base.force` function.

- Changed the deprecation warning for `mod` to recommend `( % )` rather than
  `Caml.( mod )`.

### Performance related changes

- Optimized `List.compare`, removing its closure allocation.

- Optimized `String.mem` to not allocate.

- Optimized `Float.is_negative`, `is_non_negative`, `is_positive`, and
  `is_non_positive` to avoid some boxing.

- Changed `Hashtbl.merge` to relax its equality check on the input tables'
  `Hashable.t` records, checking physical equality componentwise if the records
  aren't physically equal.

- Added `Result.combine_errors`, similar to `Or_error.combine_errors`, with a
  slightly different type.

- Added `Result.combine_errors_unit`, similar to `Or_error.combine_errors_unit`.

- Optimized the `With_return.return` type by adding the `[@@unboxed]` attribute.

- Improved a number of deprecation warnings.


## v0.9

Initial release.
