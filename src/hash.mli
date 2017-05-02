open! Import0

module F (Hash : Hash_intf.S) :
  Hash_intf.Full
  with type hash_value = Hash.hash_value
   and type state      = Hash.state
   and type seed       = Hash.seed

(**
   The code of ppx_hash is agnostic to the choice of hash algorithm that is used. However,
   it is not currently possible to mix various choices of hash algorithms in a given code
   base.

   We experimented with:
   - custom hash algorithms implemented in OCaml (a) and C (b);
   - OCaml's internal hash function (c) (which is a custom version of Murmur3, implemented
   in C)
   - siphash, a modern hash function implemented in C (d).

   Our findings were as follows:

   - Implementing our own custom hash algorithms in OCaml and C yielded very little
   performance improvement over the (c) proposal, without providing the benefit of being a
   peer-reviewed, widely used hash function.

   - Siphash (a modern hash function with an internal state of 32 bytes) has a worse
   performance profile than (a,b,c) above (hashing takes more time). Since its internal
   state is bigger than an OCaml immediate value, one must either manage allocation of
   such state explicitly, or paying the cost of allocation each time a hash is computed.
   While being a supposedly good hash function (with good hash quality), this quality was
   not translated in measurable improvemenets in our macro benchmarks. (Also, based on the
   data available at the time of writing, it's unclear that other hash algorithms in this
   class would be more than marginally faster.)

   - By contrast, using the internal combinators of OCaml hash function means that we do
   not allocate (the internal state of this hash function is 32 bit) and have the same
   quality and performance as Hashtbl.hash.

   Hence, we are here making the choice of using this Internalhash (that is, Murmur3, the
   OCaml hash algorithm as of 4.03) as our hash algorithm. It means that the state of the
   hash function does not need to be preallocated, and makes for simpler use in hash
   tables and other structures. *)

include Hash_intf.Full
  with type state      = private int
   and type seed       = int

   and type hash_value = int (** @open *)
