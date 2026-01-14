open! Import

module Definitions = struct
  (** Interface for Unicode encodings, such as UTF-8. Written with an abstract type, and
      specialized below. *)
  module type Utf = sig
    type t [@@deriving sexp_grammar]

    (** [t_of_sexp] and [of_string] will raise if the input is invalid in this encoding.
        See [sanitize] below to construct a valid [t] from arbitrary input. *)
    include%template Identifiable.S [@mode local] [@modality portable] with type t := t

    (** Interpret [t] as a container of Unicode scalar values, rather than of ASCII
        characters. Indexes, length, etc. are with respect to [Uchar.t]. *)
    include Indexed_container.S0_with_creators with type t := t and type elt = Uchar0.t

    (** Produce a sequence of unicode characters. *)
    val to_sequence : t -> Uchar0.t Sequence.t

    (** Reports whether a string is valid in this encoding. *)
    val is_valid : local_ string -> bool

    (** Create a [t] from a string by replacing any byte sequences that are invalid in
        this encoding with [Uchar.replacement_char]. This can be used to decode strings
        that may be encoded incorrectly. *)
    val sanitize : string -> t

    (** Decodes the Unicode scalar value at the given byte index in this encoding. Raises
        if [byte_pos] does not refer to the start of a Unicode scalar value. *)
    val get : t -> byte_pos:int -> Uchar0.t

    (** Creates a [t] without sanitizing or validating the string. Other functions in this
        interface may raise or produce unpredictable results if the string is invalid in
        this encoding. *)
    val of_string_unchecked : string -> t

    (** Attempts to decode the Unicode scalar value at the given byte index in this
        encoding, without first sanitizing or validating the string. *)
    val get_unchecked : local_ string -> byte_pos:int -> Uchar0.utf_decode

    (** Similar to [String.split], but splits on a [Uchar.t] in [t]. If you want to split
        on a [char], first convert it with [Uchar.of_char], but note that the actual
        byte(s) on which [t] is split may not be the same as the [char] byte depending on
        both [char] and the encoding of [t]. For example, splitting on 'α' in UTF-8 or on
        '\n' in UTF-16 is actually splitting on a 2-byte sequence. *)
    val split : t -> on:Uchar0.t -> t list

    (** The name of this encoding scheme; e.g., "UTF-8". *)
    val codec_name : string

    (** Counts the number of unicode scalar values in [t].

        This function is not a good proxy for display width, as some scalar values have
        display widths > 1. Many native applications such as terminal emulators use
        [wcwidth] (see [man 3 wcwidth]) to compute the display width of a scalar value.
        See the uucp library's [Uucp.Break.tty_width_hint] for an implementation of
        [wcwidth]'s logic. However, this is merely best-effort, as display widths will
        vary based on the font and underlying text shaping engine (see docs on
        [tty_width_hint] for details).

        For applications that support Grapheme clusters (many terminal emulators do not),
        [t] should first be split into Grapheme clusters and then the display width of
        each of those Grapheme clusters needs to be computed (which is the max display
        width of the scalars that are in the cluster).

        There are some active efforts to improve the current state of affairs:
        - https://github.com/wez/wezterm/issues/4320
        - https://www.unicode.org/L2/L2023/23194-text-terminal-wg-report.pdf *)
    val length_in_uchars : t -> int

    (** [length] could be misinterpreted as counting bytes. We direct users to other,
        clearer options. *)
    val length : t -> int
    [@@alert
      length_in_uchars
        "Use [length_in_uchars] to count unicode scalar values or [String.length] to \
         count bytes"]
  end

  (** Iterface for Unicode encodings, specialized for string representation. *)
  module type Utf_as_string = Utf with type t = private string
end

module type String = sig @@ portable
  include module type of struct
    include Definitions
  end

  (** An extension of the standard [StringLabels]. If you [open Base], you'll get these
      extensions in the [String] module. *)

  open! Import

  type t = string [@@deriving globalize, sexp ~stackify, sexp_grammar]

  [%%template:
  [@@@alloc.default a @ m = (heap @ global, stack @ local)]

  val sub : t @ m -> pos:int -> len:int -> t @ m

  (** [sub] with no bounds checking, and always returns a new copy *)
  val unsafe_sub : t @ local -> pos:int -> len:int -> t @ m

  val subo : ?pos:int -> ?len:int -> t @ m -> t @ m]

  include%template
    Indexed_container.S0_with_creators
    [@alloc stack]
    with type t := t
    with type elt = char

  include%template Identifiable.S [@mode local] [@modality portable] with type t := t

  include Invariant.S with type t := t

  (** Maximum length of a string. *)
  val max_length : int

  val mem : local_ t -> char -> bool
  external length : (t[@local_opt]) -> int = "%string_length"
  external get : (t[@local_opt]) -> (int[@local_opt]) -> char = "%string_safe_get"

  (** [unsafe_get t i] is like [get t i] but does not perform bounds checking. The caller
      must ensure that it is a memory-safe operation. *)
  external unsafe_get
    :  (string[@local_opt])
    -> (int[@local_opt])
    -> char
    = "%string_unsafe_get"

  val%template make : int -> char -> t @ m
  [@@alloc a @ m = (heap @ global, stack @ local)]

  (** String append. Also available unqualified, but re-exported here for documentation
      purposes.

      Note that [a ^ b] must copy both [a] and [b] into a newly-allocated result string,
      so [a ^ b ^ c ^ ... ^ z] is quadratic in the number of strings. [String.concat] does
      not have this problem -- it allocates the result buffer only once. *)
  val ( ^ ) : t @ local -> t @ local -> t

  [%%template:
  [@@@alloc.default a @ m = (heap @ global, stack @ local)]

  val append : t @ local -> t @ local -> t @ m

  (** Concatenates all strings in the list using separator [sep] (with a default separator
      [""]). *)
  val concat : ?sep:t -> t list @ local -> t @ m]

  (** Special characters are represented by escape sequences, following the lexical
      conventions of OCaml. *)
  val escaped : t -> t

  val contains : ?pos:int -> ?len:int -> local_ t -> char -> bool

  (** Operates on the whole string using the US-ASCII character set, e.g.
      [uppercase "foo" = "FOO"]. *)
  val uppercase : t -> t

  val uppercase__stack : t @ local -> t @ local

  (** Operates on the whole string using the US-ASCII character set, e.g.
      [lowercase "FOO" = "foo"]. *)
  val lowercase : t -> t

  val lowercase__stack : t @ local -> t @ local

  (** Operates on just the first character using the US-ASCII character set, e.g.
      [capitalize "foo" = "Foo"]. *)
  val capitalize : t -> t

  val uncapitalize : t -> t

  (** [Caseless] compares and hashes strings ignoring case, so that for example
      [Caseless.equal "OCaml" "ocaml"] and [Caseless.("apple" < "Banana")] are [true].

      [Caseless] also provides case-insensitive [is_suffix] and [is_prefix] functions, so
      that for example [Caseless.is_suffix "OCaml" ~suffix:"AmL"] and
      [Caseless.is_prefix "OCaml" ~prefix:"oc"] are [true]. *)
  module Caseless : sig
    type nonrec t = t [@@deriving hash, sexp ~stackify, sexp_grammar]

    include%template Comparable.S [@modality portable] with type t := t

    include Ppx_compare_lib.Comparable.S__local with type t := t

    val is_suffix : local_ t -> suffix:local_ t -> bool
    val is_prefix : local_ t -> prefix:local_ t -> bool
    val is_substring : local_ t -> substring:local_ t -> bool
    val is_substring_at : local_ t -> pos:int -> substring:local_ t -> bool
    val substr_index : ?pos:int -> local_ t -> pattern:local_ t -> int option
    val substr_index_exn : ?pos:int -> local_ t -> pattern:t -> int
    val substr_index_all : local_ t -> may_overlap:bool -> pattern:local_ t -> int list

    [%%template:
    [@@@alloc.default a @ m = (heap @ global, stack @ local)]

    val substr_replace_first
      :  ?pos:int
      -> t @ m
      -> pattern:t @ local
      -> with_:t @ local
      -> t @ m

    val substr_replace_all : t @ m -> pattern:t @ local -> with_:t @ local -> t @ m]
  end

  (** [index] gives the index of the first appearance of [char] in the string when
      searching from left to right, or [None] if it's not found. [rindex] does the same
      but searches from the right.

      For example, [String.index "Foo" 'o'] is [Some 1] while [String.rindex "Foo" 'o'] is
      [Some 2].

      The [_exn] versions return the actual index (instead of an option) when [char] is
      found, and raise [Stdlib.Not_found] or [Not_found_s] otherwise. *)

  val index : t -> char -> int option
  val index_exn : t -> char -> int
  val index_from : t -> int -> char -> int option
  val index_from_exn : t -> int -> char -> int
  val rindex : t -> char -> int option
  val rindex_exn : t -> char -> int
  val rindex_from : t -> int -> char -> int option
  val rindex_from_exn : t -> int -> char -> int

  (** Produce a sequence of the characters in a string. *)
  val to_sequence : t -> char Sequence.t

  (** Read the characters in a full sequence and produce a string. *)
  val of_sequence : char Sequence.t -> t

  (** Substring search and replace functions. They use the Knuth-Morris-Pratt algorithm
      (KMP) under the hood.

      The functions in the [Search_pattern] module allow the program to preprocess the
      searched pattern once and then use it many times without further allocations. *)
  module Search_pattern : sig
    type t : immutable_data [@@deriving sexp_of ~stackify]

    (** [create pattern] preprocesses [pattern] as per KMP, building an [int array] of
        length [length pattern]. All inputs are valid. *)
    val%template create
      :  ?case_sensitive:bool (** default = true *)
      -> string @ m
      -> t @ m
    [@@alloc a @ m = (heap @ global, stack @ local)]

    (** [pattern t] returns the string pattern used to create [t]. *)
    val%template pattern : t @ m -> string @ m
    [@@mode m = (global, local)]

    (** [case_sensitive t] returns whether [t] matches strings case-sensitively. *)
    val case_sensitive : local_ t -> bool

    (** [matches pat str] returns true if [str] matches [pat] *)
    val matches : local_ t -> local_ string -> bool

    (** [pos < 0] or [pos >= length string] result in no match (hence [index] returns
        [None] and [index_exn] raises). *)
    val index : ?pos:int -> local_ t -> in_:local_ string -> int option

    val index_exn : ?pos:int -> t -> in_:local_ string -> int

    (** [may_overlap] determines whether after a successful match, [index_all] should
        start looking for another one at the very next position ([~may_overlap:true]), or
        jump to the end of that match and continue from there ([~may_overlap:false]),
        e.g.:

        - [index_all (create "aaa") ~may_overlap:false ~in_:"aaaaBaaaaaa" = [0; 5; 8]]
        - [index_all (create "aaa") ~may_overlap:true ~in_:"aaaaBaaaaaa" = [0; 1; 5; 6; 7; 8]]

        E.g., [replace_all] internally calls [index_all ~may_overlap:false]. *)
    val index_all : local_ t -> may_overlap:bool -> in_:local_ string -> int list

    [%%template:
    [@@@alloc.default a @ m = (heap @ global, stack @ local)]

    val replace_first
      :  ?pos:int
      -> t @ local
      -> in_:string @ m
      -> with_:string @ local
      -> string @ m

    (** [replace_all pattern ~in_:text ~with_:r] replaces every appearance of a [pattern]
        in [text]. Surprisingly, the result can still contain [pattern] at the end, as
        shown in the following example.

        {[
          # let pattern = String.Search_pattern.create "bc"
          val pattern : String.Search_pattern.t = <abstr>
          # String.Search_pattern.replace_all pattern ~in_:"aabbcc" ~with_:"cb"
          - : string = "aabcbc"
        ]}

        which ends with ["bc"]! *)
    val replace_all : t @ local -> in_:string @ m -> with_:string @ local -> string @ m]

    (** Similar to [String.split] or [String.split_on_chars], but instead uses a given
        search pattern as the separator. Separators are non-overlapping. *)
    val split_on : local_ t -> string -> string list

    (**/**)

    (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

        https://opensource.janestreet.com/standards/#private-submodules *)
    module Private : sig
      type public = t

      type t =
        { pattern : string
        ; case_sensitive : bool
        ; kmp_array : int iarray
        }
      [@@deriving equal ~localize, sexp_of ~stackify]

      val representation : public -> t
    end
  end

  (** Substring search and replace convenience functions. They call
      [Search_pattern.create] and then forget the preprocessed pattern when the search is
      complete. [pos < 0] or [pos >= length t] result in no match (hence [substr_index]
      returns [None] and [substr_index_exn] raises). [may_overlap] indicates whether to
      report overlapping matches, see [Search_pattern.index_all]. *)
  val substr_index : ?pos:int -> local_ t -> pattern:local_ t -> int option

  val substr_index_exn : ?pos:int -> local_ t -> pattern:t -> int
  val substr_index_all : local_ t -> may_overlap:bool -> pattern:local_ t -> int list

  [%%template:
  [@@@alloc.default a @ m = (heap @ global, stack @ local)]

  val substr_replace_first
    :  ?pos:int
    -> t @ m
    -> pattern:t @ local
    -> with_:t @ local
    -> t @ m

  (** As with [Search_pattern.replace_all], the result may still contain [pattern]. *)
  val substr_replace_all : t @ m -> pattern:t @ local -> with_:t @ local -> t @ m]

  (** [is_substring ~substring:"bar" "foo bar baz"] is true. *)
  val is_substring : local_ t -> substring:local_ t -> bool

  (** [is_substring_at "foo bar baz" ~pos:4 ~substring:"bar"] is true. *)
  val is_substring_at : local_ t -> pos:int -> substring:local_ t -> bool

  (** Returns the reversed list of characters contained in a list. *)
  val to_list_rev : t -> char list

  (** [rev t] returns [t] in reverse order. *)
  val rev : t -> t

  (** [is_suffix s ~suffix] returns [true] if [s] ends with [suffix]. *)
  val is_suffix : local_ t -> suffix:local_ t -> bool

  (** [is_prefix s ~prefix] returns [true] if [s] starts with [prefix]. *)
  val is_prefix : local_ t -> prefix:local_ t -> bool

  (** If the string [s] contains the character [on], then [lsplit2_exn s ~on] returns a
      pair containing [s] split around the first appearance of [on] (from the left).
      Raises [Stdlib.Not_found] or [Not_found_s] when [on] cannot be found in [s]. *)
  val lsplit2_exn : t -> on:char -> t * t

  (** If the string [s] contains the character [on], then [rsplit2_exn s ~on] returns a
      pair containing [s] split around the first appearance of [on] (from the right).
      Raises [Stdlib.Not_found] or [Not_found_s] when [on] cannot be found in [s]. *)
  val rsplit2_exn : t -> on:char -> t * t

  (** [lsplit2 s ~on] optionally returns [s] split into two strings around the first
      appearance of [on] from the left. *)
  val lsplit2 : t -> on:char -> (t * t) option

  (** [rsplit2 s ~on] optionally returns [s] split into two strings around the first
      appearance of [on] from the right. *)
  val rsplit2 : t -> on:char -> (t * t) option

  [%%template:
  [@@@alloc.default a @ m = (heap @ global, stack @ local)]

  (** [split s ~on] returns a list of substrings of [s] that are separated by [on].
      Consecutive [on] characters will cause multiple empty strings in the result.
      Splitting the empty string returns a list of the empty string, not the empty list. *)
  val split : t @ m -> on:char -> t list @ m

  (** [split_on_chars s ~on] returns a list of all substrings of [s] that are separated by
      one of the chars from [on]. [on] are not grouped. So a grouping of [on] in the
      source string will produce multiple empty string splits in the result. *)
  val split_on_chars : t @ m -> on:char list -> t list @ m]

  (** [split_lines t] returns the list of lines that comprise [t]. The lines do not
      include the trailing ["\n"] or ["\r\n"]. *)
  val split_lines : t -> t list

  (** [lfindi ?pos t ~f] returns the smallest [i >= pos] such that [f i t.[i]], if there
      is such an [i]. By default, [pos = 0]. *)
  val lfindi : ?pos:int -> local_ t -> f:local_ (int -> char -> bool) -> int option

  (** [rfindi ?pos t ~f] returns the largest [i <= pos] such that [f i t.[i]], if there is
      such an [i]. By default [pos = length t - 1]. *)
  val rfindi : ?pos:int -> local_ t -> f:local_ (int -> char -> bool) -> int option

  (** [lstrip ?drop s] returns a string with consecutive chars satisfying [drop] (by
      default white space, e.g. tabs, spaces, newlines, and carriage returns) stripped
      from the beginning of [s]. *)
  val lstrip : ?drop:local_ (char -> bool) -> t -> t

  (** [rstrip ?drop s] returns a string with consecutive chars satisfying [drop] (by
      default white space, e.g. tabs, spaces, newlines, and carriage returns) stripped
      from the end of [s]. *)
  val rstrip : ?drop:local_ (char -> bool) -> t -> t

  (** [strip ?drop s] returns a string with consecutive chars satisfying [drop] (by
      default white space, e.g. tabs, spaces, newlines, and carriage returns) stripped
      from the beginning and end of [s]. *)
  val strip : ?drop:local_ (char -> bool) -> t -> t

  [%%template:
  [@@@mode.default mi = (global, local)]
  [@@@alloc.default a @ mo = (heap_global, stack_local)]

  (** Like [map], but allows the replacement of a single character with zero or two or
      more characters. *)
  val concat_map : ?sep:t @ mi -> t @ mi -> f:local_ (char -> t @ mo) -> t @ mo

  val concat_mapi : ?sep:t @ mi -> t @ mi -> f:local_ (int -> char -> t @ mo) -> t @ mo]

  (** [tr ~target ~replacement s] replaces every instance of [target] in [s] with
      [replacement]. *)
  val%template tr : target:char -> replacement:char -> t @ m -> t @ m
  [@@alloc a @ m = (heap @ global, stack @ local)]

  (** [tr_multi ~target ~replacement] returns a function that replaces every instance of a
      character in [target] with the corresponding character in [replacement].

      If [replacement] is shorter than [target], it is lengthened by repeating its last
      character. Empty [replacement] is illegal unless [target] also is.

      If [target] contains multiple copies of the same character, the last corresponding
      [replacement] character is used. Note that character ranges are {b not} supported,
      so [~target:"a-z"] means the literal characters ['a'], ['-'], and ['z']. *)
  val tr_multi : target:t -> replacement:t -> (t -> t) Staged.t

  (** [chop_suffix_exn s ~suffix] returns [s] without the trailing [suffix], raising
      [Invalid_argument] if [suffix] is not a suffix of [s]. *)
  val chop_suffix_exn : t -> suffix:t -> t

  (** [chop_prefix_exn s ~prefix] returns [s] without the leading [prefix], raising
      [Invalid_argument] if [prefix] is not a prefix of [s]. *)
  val chop_prefix_exn : t -> prefix:t -> t

  val chop_suffix : t -> suffix:t -> t option
  val chop_prefix : t -> prefix:t -> t option

  (** [chop_suffix_if_exists s ~suffix] returns [s] without the trailing [suffix], or just
      [s] if [suffix] isn't a suffix of [s].

      Equivalent to [chop_suffix s ~suffix |> Option.value ~default:s], but avoids
      allocating the intermediate option. *)
  val chop_suffix_if_exists : t -> suffix:t -> t

  (** [chop_prefix_if_exists s ~prefix] returns [s] without the leading [prefix], or just
      [s] if [prefix] isn't a prefix of [s].

      Equivalent to [chop_prefix s ~prefix |> Option.value ~default:s], but avoids
      allocating the intermediate option. *)
  val chop_prefix_if_exists : t -> prefix:t -> t

  (** [suffix s n] returns the longest suffix of [s] of length less than or equal to [n]. *)
  val suffix : t -> int -> t

  (** [prefix s n] returns the longest prefix of [s] of length less than or equal to [n]. *)
  val prefix : t -> int -> t

  (** [drop_suffix s n] drops the longest suffix of [s] of length less than or equal to
      [n]. *)
  val drop_suffix : t -> int -> t

  (** [drop_prefix s n] drops the longest prefix of [s] of length less than or equal to
      [n]. *)
  val drop_prefix : t -> int -> t

  (** Produces the longest common suffix, or [""] if the list is empty. *)
  val common_suffix : t list -> t

  (** Produces the longest common prefix, or [""] if the list is empty. *)
  val common_prefix : t list -> t

  (** Produces the length of the longest common suffix, or 0 if the list is empty. *)
  val common_suffix_length : t list -> int

  (** Produces the length of the longest common prefix, or 0 if the list is empty. *)
  val common_prefix_length : t list -> int

  (** Produces the longest common suffix. *)
  val common_suffix2 : t -> t -> t

  (** Produces the longest common prefix. *)
  val common_prefix2 : t -> t -> t

  (** Produces the length of the longest common suffix. *)
  val common_suffix2_length : t -> t -> int

  (** Produces the length of the longest common prefix. *)
  val common_prefix2_length : t -> t -> int

  (** [concat_array sep ar] like {!String.concat}, but operates on arrays. *)
  val concat_array : ?sep:local_ t -> local_ t array -> t

  (** Builds a multiline text from a list of lines. Each line is terminated and then
      concatenated.

      {[
        # String.concat_lines ["one two"; "three four"; "five"]
        - : string = "one two\nthree four\nfive\n"
        # String.concat_lines ~crlf:true ["one two"; "three four"; "five"]
        - : string = "one two\r\nthree four\r\nfive\r\n"
      ]} *)
  val concat_lines : ?crlf:bool (** default [false] *) -> string list -> string

  (** Slightly faster hash function on strings. *)
  external hash : t -> int = "Base_hash_string"
  [@@noalloc]

  val of_char : char -> t
  val of_char_list : char list -> t

  (** [pad_left ?char s ~len] returns [s] padded to the length [len] by adding characters
      [char] to the beginning of the string. If s is already longer than [len] it is
      returned unchanged. *)
  val pad_left : ?char:char (** default is [' '] *) -> string -> len:int -> string

  (** [pad_right ?char ~s len] returns [s] padded to the length [len] by adding characters
      [char] to the end of the string. If s is already longer than [len] it is returned
      unchanged. *)
  val pad_right : ?char:char (** default is [' '] *) -> string -> len:int -> string

  (** Reports the Levenshtein edit distance between two strings. Computes the minimum
      number of single-character insertions, deletions, and substitutions needed to
      transform one into the other.

      For strings of length M and N, its time complexity is O(M*N) and its space
      complexity is O(min(M,N)). *)
  val edit_distance : string -> string -> int

  (** Operations for escaping and unescaping strings, with parameterized escape and
      escapeworthy characters. Escaping/unescaping using this module is more efficient
      than using Pcre. Benchmark code can be found in core/benchmarks/string_escaping.ml. *)
  module Escaping : sig
    (** [escape_gen_exn escapeworthy_map escape_char] returns a (staged) function that
        will escape a string [s] as follows: if [(c1,c2)] is in [escapeworthy_map], then
        all occurrences of [c1] are replaced by [escape_char] concatenated to [c2].

        Raises an exception if [escapeworthy_map] is not one-to-one. If [escape_char] is
        not in [escapeworthy_map], then it will be escaped to itself.

        Examples:

        {[
          # let escape = Staged.unstage (String.Escaping.escape_gen_exn ~escapeworthy_map:['a','b'; 'b','c'] ~escape_char:'!')
          val escape : string -> string = <fun>
          # escape "a!bcd";
          - : string = "!b!!!ccd"
          # escape "efgh";
          - : string = "efgh"
          # let escape = Staged.unstage (String.Escaping.escape_gen_exn ~escapeworthy_map:['a','b'; 'c','b'] ~escape_char:'!')
          Exception:
          ("escapeworthy_map not one-to-one" (c_from c) (c_to b)
            (escapeworthy_map ((! !) (a b) (c b))))
        ]} *)
    val escape_gen_exn
      :  escapeworthy_map:(char * char) list
      -> escape_char:char
      -> (string -> string) Staged.t @ portable

    (** Like {!escape_gen_exn}, but returns an [Or_error.t] when constructing the escaping
        function, rather than raising. *)
    val escape_gen
      :  escapeworthy_map:(char * char) list
      -> escape_char:char
      -> (string -> string) Or_error.t @ portable

    (** A simpler version of {!escape_gen}. In this function, any escaped character is
        escaped to itself. I.e., if the escape character is ['!'] then escaping the
        character ['a'] will generate ["!a"].

        Duplicates will be removed from [escapeworthy], and the escape character is
        implicitly considered escapeworthy, whether or not its on the list explicitly.

        An example where we include the escape character explicitly as escapeworthy:

        {[
          # let escape = Staged.unstage (String.Escaping.escape ~escapeworthy:['a';'b';'c';'!'] ~escape_char:'!')
          val escape : string -> string = <fun>
          # escape "abcd!ef"
          - : string = "!a!b!cd!!ef"
        ]}

        And where we don't.

        {[
          # let escape = Staged.unstage (String.Escaping.escape ~escapeworthy:['a';'b';'c'] ~escape_char:'!')
          val escape : string -> string = <fun>
          # escape "abcd!ef"
          - : string = "!a!b!cd!!ef"
        ]} *)
    val escape
      :  escapeworthy:char list
      -> escape_char:char
      -> (string -> string) Staged.t @ portable

    (** [unescape_gen_exn] is the inverse operation of [escape_gen_exn].

        Example:

        {[
          # let escapeworthy_map = ['a','b'; 'b','c'] and escape_char = '!'
          val escapeworthy_map : (char * char) list = [('a', 'b'); ('b', 'c')]
          val escape_char : char = '!'
          # let escape = Staged.unstage (String.Escaping.escape_gen_exn ~escapeworthy_map ~escape_char)
          val escape : string -> string = <fun>
          # let unescape = Staged.unstage (String.Escaping.unescape_gen_exn ~escapeworthy_map ~escape_char)
          val unescape : string -> string = <fun>
          # unescape (escape "abc!de")
          - : string = "abc!de"
        ]} *)
    val unescape_gen_exn
      :  escapeworthy_map:(char * char) list
      -> escape_char:char
      -> (string -> string) Staged.t @ portable

    val unescape_gen
      :  escapeworthy_map:(char * char) list
      -> escape_char:char
      -> (string -> string) Or_error.t @ portable

    (** [unescape ~escape_char] is defined as [unescape_gen_exn ~map:[] ~escape_char] *)
    val unescape : escape_char:char -> (string -> string) Staged.t @ portable

    (** Any char in an escaped string is either escaping, escaped, or literal. For
        example, for escaped string ["0_a0__0"] with [escape_char] as ['_'], pos 1 and 4
        are escaping, 2 and 5 are escaped, and the rest are literal.

        [is_char_escaping s ~escape_char pos] returns true if the char at [pos] is
        escaping, false otherwise. *)
    val is_char_escaping : string -> escape_char:char -> int -> bool

    (** [is_char_escaped s ~escape_char pos] returns true if the char at [pos] is escaped,
        false otherwise. *)
    val is_char_escaped : string -> escape_char:char -> int -> bool

    (** [is_char_literal s ~escape_char pos] returns true if the char at [pos] is not
        escaped or escaping. *)
    val is_char_literal : string -> escape_char:char -> int -> bool

    (** [index s ~escape_char char] finds the first literal (not escaped) instance of
        [char] in s starting from 0. *)
    val index : string -> escape_char:char -> char -> int option

    val index_exn : string -> escape_char:char -> char -> int

    (** [rindex s ~escape_char char] finds the first literal (not escaped) instance of
        [char] in [s] starting from the end of [s] and proceeding towards 0. *)
    val rindex : string -> escape_char:char -> char -> int option

    val rindex_exn : string -> escape_char:char -> char -> int

    (** [index_from s ~escape_char pos char] finds the first literal (not escaped)
        instance of [char] in [s] starting from [pos] and proceeding towards the end of
        [s]. *)
    val index_from : string -> escape_char:char -> int -> char -> int option

    val index_from_exn : string -> escape_char:char -> int -> char -> int

    (** [rindex_from s ~escape_char pos char] finds the first literal (not escaped)
        instance of [char] in [s] starting from [pos] and towards 0. *)
    val rindex_from : string -> escape_char:char -> int -> char -> int option

    val rindex_from_exn : string -> escape_char:char -> int -> char -> int

    (** [split s ~escape_char ~on] returns a list of substrings of [s] that are separated
        by literal versions of [on]. Consecutive [on] characters will cause multiple empty
        strings in the result. Splitting the empty string returns a list of the empty
        string, not the empty list.

        E.g., [split ~escape_char:'_' ~on:',' "foo,bar_,baz" = ["foo"; "bar_,baz"]]. *)
    val split : string -> on:char -> escape_char:char -> string list

    (** [split_on_chars s ~on] returns a list of all substrings of [s] that are separated
        by one of the literal chars from [on]. [on] are not grouped. So a grouping of [on]
        in the source string will produce multiple empty string splits in the result.

        E.g.,
        [split_on_chars ~escape_char:'_' ~on:[',';'|'] "foo_|bar,baz|0" -> ["foo_|bar"; "baz"; "0"]]. *)
    val split_on_chars : string -> on:char list -> escape_char:char -> string list

    (** [lsplit2 s ~on ~escape_char] splits s into a pair on the first literal instance of
        [on] (meaning the first unescaped instance) starting from the left. *)
    val lsplit2 : string -> on:char -> escape_char:char -> (string * string) option

    val lsplit2_exn : string -> on:char -> escape_char:char -> string * string

    (** [rsplit2 s ~on ~escape_char] splits [s] into a pair on the first literal instance
        of [on] (meaning the first unescaped instance) starting from the right. *)
    val rsplit2 : string -> on:char -> escape_char:char -> (string * string) option

    val rsplit2_exn : string -> on:char -> escape_char:char -> string * string

    (** These are the same as [lstrip], [rstrip], and [strip] for generic strings, except
        that they only drop literal characters -- they do not drop characters that are
        escaping or escaped. This makes sense if you're trying to get rid of junk
        whitespace (for example), because escaped whitespace seems more likely to be
        deliberate and not junk. *)
    val lstrip_literal : ?drop:local_ (char -> bool) -> t -> escape_char:char -> t

    val rstrip_literal : ?drop:local_ (char -> bool) -> t -> escape_char:char -> t
    val strip_literal : ?drop:local_ (char -> bool) -> t -> escape_char:char -> t
  end

  (** UTF-8 encoding. See [Utf] interface. *)
  module Utf8 : Utf_as_string

  (** UTF-16 little-endian encoding. See [Utf] interface. *)
  module Utf16le : Utf_as_string

  (** UTF-16 big-endian encoding. See [Utf] interface. *)
  module Utf16be : Utf_as_string

  (** UTF-32 little-endian encoding. See [Utf] interface. *)
  module Utf32le : Utf_as_string

  (** UTF-32 big-endian encoding. See [Utf] interface. *)
  module Utf32be : Utf_as_string
end
