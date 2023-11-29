(** This module defines types for the pretty printer. *)

module type CostFactory =
sig
  (** The cost factory interface.

      A valid cost factory should also satisfy the following contracts.

      {ul {- [le] is a total ordering.}
          {- If [le a b] and [le c d] then [le (combine a c) (combine b d)]}
          {- If [a] <= [b], then [le (text a l) (text b l)]}
          {- If [a] <= [b], then [le (newline a) (newline b)]}
          {- [text c (a + b) = combine (text c a) (text (c + a) b)]}
          {- [combine] is associative and has the identity equal to [text 0 0]}
          {- [text c 0] = [text 0 0] for any [c]}}

      See {{!Printer.default_cost_factory}[default_cost_factory]},
      {{!page-index.factory}the cost factory section},
      and {{: https://dl.acm.org/doi/abs/10.1145/3622837 }the paper}
      for examples of cost factories. *)

  type t
  (** A type for cost *)

  val text : int -> int -> t
  (** [text c l] calculates a cost for a text placement at column position [c]
      with length [l] *)

  val newline : int -> t
  (** [newline i] calculates a cost for a newline and indentation at level [i] *)

  val combine : t -> t -> t
  (** [combine x y] combines the costs [x] and [y] together *)

  val le : t -> t -> bool
  (** [le x y] tests if the cost [x] is less than or equal to the cost [y]. *)

  val limit: int
  (** [limit] is {{!page-index.complimit}the computation width limit}. *)

  val debug : t -> string
  (** [debug c] produces a string representation of a cost [c] *)

end

module type PrinterT =
sig

  type doc
  (** The [doc] type *)

  type cost
  (** The [cost] type *)

  (** Examples in the rest of this section assume that the program begins with

      {[
      open Pretty_expressive

      let cf = Printer.default_cost_factory ~page_width:10 ()
      module P = Printer.Make (val cf)
      open P
      ]} *)


  (** {2 Text document} *)

  val text : string -> doc
  (** [text s] is a document for textual content [s];
      [s] must not contain a newline.

      {5 Examples:}
      {[
# pretty_print (text "Portal") |> print_endline;;
Portal
- : unit = ()
      ]} *)

  (** {2 Newline documents} *)

  val newline : (string option) -> doc
  (** [newline s] is a document for a newline.
      When [s] is [None], it {!flatten}s to {!fail}.
      When [s] is not [None], it {!flatten}s to [text s].
      See {!flatten} for more details. *)

  val nl : doc
  (** [nl] is a document for a newline that {!flatten}s to a single space. *)

  val break : doc
  (** [break] is a document for a newline that {!flatten}s to an empty string. *)

  val hard_nl : doc
  (** [hard_nl] is a document for a newline that {!fail}s to {!flatten}. *)

  (** {2 Concatenation document} *)

  val (^^) : doc -> doc -> doc
  (** [a ^^ b] is a document for concatenation of documents [a] and [b]
      {i without} alignment.
      In the paper, the symbol [<>] is used for the operator.
      We use [^^] in the OCaml implementation instead to avoid shadowing
      the built-in not equal operator.
      This operator also known as the {i unaligned concatenation},
      which is widely used in traditional pretty printers.

      See also {!Printer.MakeCompat} for a functor that provides this operator
      under the symbol [<>].

      {5 Examples:}
      {[
      let left_doc = text "Splatoon" ^^ nl ^^ text "Nier";;
      let right_doc = text "Automata" ^^ nl ^^ text "FEZ";;
      ]}

      {[
      # pretty_print (left_doc ^^ right_doc) |> print_endline;;
      Splatoon
      NierAutomata
      FEZ
      - : unit = ()
      ]}

      By "without alignment," we mean that the right document is not treated as
      as box with a rigid structure. This makes it easy to format code in
      C-like languages, whose array expression, function call, and curly braces
      should not be rigid.
 *)

  (** {2 Choice document} *)

  val (<|>) : doc -> doc -> doc
  (** [a <|> b] is a document for a choice between document [a] and [b].

      {[
# let print_doc w =
    let cf = Printer.default_cost_factory ~page_width:w () in
    let module P = Printer.Make (val cf) in
    let open P in
    pretty_print (text "Chrono Trigger" <|>
                 (text "Octopath" ^^ nl ^^ text "Traveler")) |> print_endline;;
val print_doc : int -> unit = <fun>
# print_doc 10;;
Octopath
Traveler
- : unit = ()
# print_doc 15;;
Chrono Trigger
- : unit = ()
      ]}

      See also {{!page-index.bestpractice}Best Practice for Document Construction} *)

  (** {2 Indentation documents} *)

  val align : doc -> doc
  (** [align d] is a document that aligns [d] at the column position.

      {5 Examples:}
      {[
# pretty_print (left_doc ^^ align right_doc) |> print_endline;;
Splatoon
NierAutomata
    FEZ
- : unit = ()
      ]}

      The aligned concatenation operator {!(<+>)} is a derived combinator that
      composes {!(^^)} and [align] together. It is especially useful for
      languages that uses the the box model for code styling. *)

  val nest : int -> doc -> doc
  (** [nest n d] is a document that increments the indentation level by [n]
      when rendering [d].

      {5 Examples:}
      {[
# pretty_print (text "when 1 = 2:" ^^ nest 4 (nl ^^ text "print 'oh no!'"))
  |> print_endline;;
when 1 = 2:
    print 'oh no!'
- : unit = ()
      ]}

      The increment does not affect content on the current line.
      In the following example, [when 1 = 2:] is not further indented.

      {[
# pretty_print (nest 4 (text "when 1 = 2:" ^^ nl ^^ text "print 'oh no!'"))
  |> print_endline;;
when 1 = 2:
    print 'oh no!'
- : unit = ()
      ]} *)

  val reset : doc -> doc
  (** [reset d] is a document that resets indentation level to 0 in [d].
      This is especially useful for formatting multi-line strings and
      multi-line comments.

      {5 Examples:}
      {[
# let s_d = reset (text "#<<EOF" ^^ nl ^^
                   text "Zelda" ^^ nl ^^
                   text "Baba is you" ^^ nl ^^
                   text "EOF");;
val s_d : doc = <abstr>
# pretty_print (text "when 1 = 2:" ^^ nest 4 (nl ^^ text "print " ^^ s_d))
  |> print_endline;;
when 1 = 2:
    print #<<EOF
Zelda
Baba is you
EOF
- : unit = ()
      ]} *)

  (** {2 Cost document} *)

  val cost : cost -> doc -> doc
  (** [cost c d] is a document that artificially adds cost [c] to [d].

      In the below example, we artificially adds overflow to [text "CrossCode"],
      making it a non-optimal choice, even though [text "CrossCode"] would have
      been the optimal choice had [cost] not been used.

      {5 Examples:}
      {[
# pretty_print (cost (1, 0) (text "CrossCode") <|>
                (text "Final" ^^ nl ^^ text "Fantasy")) |> print_endline;;
Final
Fantasy
- : unit = ()
# pretty_print (text "CrossCode" <|>
                (text "Final" ^^ nl ^^ text "Fantasy")) |> print_endline;;
CrossCode
- : unit = ()
      ]}

      [cost] is especially useful in combination with
      {{!page-index.factory}a custom cost factory}.
      See the section for further details. *)

  (** {2 Failure document} *)

  val fail : doc
  (** A document that always fails. It interacts with {!(<|>)}:
      failing branches are pruned away.

      {5 Examples:}
      {[
# pretty_print (text "Sea of Stars" ^^ fail) |> print_endline;;
Exception: Failure "fails to render".
# pretty_print ((text "Sea of Stars" ^^ fail) <|> text "Hades") |> print_endline;;
Hades
- : unit = ()
      ]} *)

  (** {2 Pretty printing functions} *)

  val print : ?init_c:int -> doc -> Util.info
  (** [print d] prints the document [d] to an [info] record.
      The optional [~init_c] can be used to indicate that the printing begins
      at a non-zero column position. *)

  val pretty_print : ?init_c:int -> doc -> string
  (** [pretty_print d] prints the document [d] to a string.
      The optional [~init_c] can be used to indicate that the printing begins
      at a non-zero column position.

      {5 Examples:}
      {[
# print_string "Languages: ";
  pretty_print (align (text "Racket" ^^ nl ^^
                       text "OCaml" ^^ nl ^^
                       text "Pyret")) |> print_endline;;
Languages: Racket
OCaml
Pyret
- : unit = ()
# print_string "Languages: ";
  pretty_print ~init_c:11
               (align (text "Racket" ^^ nl ^^
                       text "OCaml" ^^ nl ^^
                       text "Pyret")) |> print_endline;;
Languages: Racket
           OCaml
           Pyret
- : unit = ()
      ]} *)

  (** {2 Other derived combinators} *)

  val flatten : doc -> doc
  (** [flatten d] is a document that replaces newlines and indentation spaces
      with what's specified in [newline] when rendering [d].

      {5 Examples:}
      {[
# pretty_print (flatten (text "Fire Emblem" ^^ nl ^^ text "Awakening"))
  |> print_endline;;
Fire Emblem Awakening
- : unit = ()
# pretty_print (flatten (text "Mario + Rabbids" ^^ break ^^ text "Kingdom Battle"))
  |> print_endline;;
Mario + RabbidsKingdom Battle
- : unit = ()
# pretty_print (flatten (text "XCOM 2" ^^ hard_nl ^^ text "War of the Chosen"))
  |> print_endline;;
Exception: Failure "fails to render".
# pretty_print (flatten (text "Tactics Ogre" ^^
                         newline (Some ": ") ^^
                         text "Reborn"))
  |> print_endline;;
Tactics Ogre: Reborn
- : unit = ()
      ]} *)

  val group : doc -> doc
  (** [group d] is a shorthand for [d <|> flatten d].
      This combinator is a part of most traditional pretty printers. *)

  val (<+>) : doc -> doc -> doc
  (** [a <+> b] is a shorthand for [a ^^ align b].
      It is also known as the {i aligned concatenation}. *)

  val (<$>) : doc -> doc -> doc
  (** [a <$> b] is a shorthand for [a ^^ hard_nl ^^ b]. *)

  val (<->) : doc -> doc -> doc
  (** [a <-> b] is a shorthand for [flatten a <+> b].
      This is especially useful when combined with {!hard_nl} and {!(<|>)}:
      it can be used when we want to do aligned concatenation,
      but don't want the left part to have multiple lines. *)

  val fold_doc : (doc -> doc -> doc) -> doc list -> doc
  (** [fold_doc (++) ds] is a shorthand for [d_1 ++ d_2 ++ ... ++ d_n]
      where [d_1 d_2 ... d_n] are drawn from [ds]. *)

  val vcat : doc list -> doc
  (** [vcat ds] is a shorthand for [d_1 <$> d_2 <$> ... <$> d_n]
      where [d_1 d_2 ... d_n] are drawn from [ds]. *)

  val hcat : doc list -> doc
  (** [vcat ds] is a shorthand for [d_1 <-> d_2 <-> ... <-> d_n]
      where [d_1 d_2 ... d_n] are drawn from [ds]. *)

  val empty : doc
  (** Equivalent to [text ""] *)

  val space : doc
  (** Equivalent to [text " "] *)

  val comma : doc
  (** Equivalent to [text ","] *)

  val lbrack : doc
  (** Equivalent to [text "\["] *)

  val rbrack: doc
  (** Equivalent to [text "\]"] *)

  val lbrace : doc
  (** Equivalent to [text "{"] *)

  val rbrace : doc
  (** Equivalent to [text "}"] *)

  val lparen : doc
  (** Equivalent to [text "("] *)

  val rparen : doc
  (** Equivalent to [text ")"] *)

  val dquote : doc
  (** Equivalent to [text "\""] *)
end


module type PrinterCompatT =
sig
  include PrinterT
  (** @closed *)

  val (<>) : doc -> doc -> doc
  (** [<>] is the same as {!(^^)} *)
end
