(** This module defines types for the pretty printer. *)

type renderer = string -> unit
(** The type for a function to print from a pretty printer. *)

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
          {- [text c 0] = [text 0 0] for any [c]}
          {- If [a] <= [b], then [le (two_columns_overflow a) (two_columns_overflow b)]}
          {- If [a] <= [b], then [le (two_columns_bias a) (two_columns_bias b)]}}

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

  val two_columns_bias : int -> t
  (** [two_columns_bias i] is the bias cost, added to each possible
      column separator so that the leftmost column separator is preferred. *)

  val two_columns_overflow : int -> t
  (** [two_columns_overflow i] is the cost due to exceeding the column separator.
      Make this cost greater than the usual overflow cost
      (for exceeding the page width limit) to prefer going over the
      page width limit instead of going over the column separator. *)

  val limit: int
  (** [limit] is {{!page-index.complimit}the computation width limit}. *)

  val string_of_cost : t -> string
  (** [string_of_cost c] produces a string representation of a cost [c] *)

  val debug_format : string -> bool -> string -> string
  (** [debug_format s is_tainted cost] produces a debugging string
      from the output of the core printer. *)
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

  (** {2 Pretty printing functions} *)

  val pretty_print_info : ?init_c:int -> renderer -> doc -> cost Util.info
  (** [pretty_print_info renderer d] prints the document [d]
      by repeatedly calling [renderer] and outputs the debugging information
      as an [info] record.The optional [~init_c] can be used to indicate
      that the printing begins at a non-zero column position. *)

  val pretty_format_info : ?init_c:int -> doc -> string * cost Util.info
  (** [pretty_format_info] is similar to {{!Printer.Make.pretty_print_info}[pretty_print_info]},
      but it prints to a string instead of a renderer. *)

  val pretty_print : ?init_c:int -> renderer -> doc -> unit
  (** [pretty_print d] is similar to {{!Printer.Make.pretty_print_info}[pretty_print_info]}
      without debugging information.

      {5 Examples:}
      {[
      # print_string "Languages: ";
        pretty_print
          print_string
          (align (text "Racket" ^^ nl ^^
                  text "OCaml" ^^ nl ^^
                  text "Pyret"));;
      Languages: Racket
      OCaml
      Pyret
      - : unit = ()
      ]}

      {[
      # print_string "Languages: ";
        pretty_print
          ~init_c:11
          print_string
          (align (text "Racket" ^^ nl ^^
                  text "OCaml" ^^ nl ^^
                  text "Pyret"));;
      Languages: Racket
                 OCaml
                 Pyret
      - : unit = ()
      ]} *)

  val pretty_format : ?init_c:int -> doc -> string
  (** [pretty_format] is similar to {{!Printer.Make.pretty_format}[pretty_format]},
      without debugging information.

      {5 Examples:}
      {[
      # pretty_format (text "Hello World" ^^ nl ^^
                       text "Hi All!") |> print_endline;;
      Hello World
      Hi All!
      - : unit = ()
      ]} *)

  val pretty_format_debug : ?init_c:int -> doc -> string
  (** [pretty_format_debug] is similar to
      {{!Printer.Make.pretty_format_info}[pretty_format_info]},
      but the debugging information is included as a part of the output string.
      The format is customizable via {{!Signature.CostFactory.debug_format}[debug_format]}.

      {5 Examples:}
      {[
      # pretty_format_debug (text "Hello World" ^^ nl ^^
                             text "Hi All!") |> print_endline;;
      1234567890
      Hello Worl│d
      Hi All!   │

      is_tainted: false
      cost: (1 0 1)
      - : unit = ()
      ]} *)

  (** {2 Text document} *)

  val text : string -> doc
  (** [text s] is a document for textual content [s];
      [s] must not contain a newline.
      The cost for the text placement can be adjusted via
      {{!Signature.CostFactory.text}[CostFactory.text]}.

      {5 Examples:}
      {[
      # pretty_print print_string (text "Portal");;
      Portal
      - : unit = ()
      ]} *)

  (** {2 Newline documents} *)

  val newline : (string option) -> doc
  (** [newline s] is a document for a newline.
      When [s] is [None], it {!flatten}s to {!fail}.
      When [s] is not [None], it {!flatten}s to [text s].
      See {!flatten} for more details.
      After the newline is inserted, indentation will be added, according to
      the current indentation level.
      The cost of the newline and indentation spaces can be adjusted via
      {{!Signature.CostFactory.newline}[CostFactory.newline]}. *)

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
      The cost for the concatenation can be adjusted via
      {{!Signature.CostFactory.combine}[CostFactory.combine]}.

      See also {!Printer.MakeCompat} for a functor that provides this operator
      under the symbol [<>].

      {5 Examples:}
      {[
      # let left_doc = text "Splatoon" ^^ nl ^^ text "Nier";;
      val left_doc : doc = <abstr>
      ]}

      {[
      # let right_doc = text "Automata" ^^ nl ^^ text "FEZ";;
      val right_doc : doc = <abstr>
      ]}

      {[
      # pretty_print print_string (left_doc ^^ right_doc);;
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
          pretty_print
            print_string
            (text "Chrono Trigger" <|>
             (text "Octopath" ^^ nl ^^ text "Traveler"));;
      val print_doc : int -> unit = <fun>
      ]}

      {[
      # print_doc 10;;
      Octopath
      Traveler
      - : unit = ()
      ]}

      {[
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
      # pretty_print print_string (left_doc ^^ align right_doc);;
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
      # pretty_print
          print_string
          (text "when 1 = 2:" ^^ nest 4 (nl ^^ text "print 'oh no!'"));;
      when 1 = 2:
          print 'oh no!'
      - : unit = ()
      ]}

      The increment does not affect content on the current line.
      In the following example, [when 1 = 2:] is not further indented.

      {[
      # pretty_print
          print_string
          (nest 4 (text "when 1 = 2:" ^^ nl ^^ text "print 'oh no!'"));;
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
      ]}

      {[
      # pretty_print
          print_string
          (text "when 1 = 2:" ^^ nest 4 (nl ^^ text "print " ^^ s_d));;
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
      # pretty_format_debug (cost (1, 0, 0) (text "CrossCode") <|>
                            (text "Final" ^^ nl ^^ text "Fantasy")) |> print_endline;;
      1234567890
      Final     │
      Fantasy   │

      is_tainted: false
      cost: (0 0 1)
      - : unit = ()
      ]}

      {[
      # pretty_format_debug (text "CrossCode" <|>
                            (text "Final" ^^ nl ^^ text "Fantasy")) |> print_endline;;
      1234567890
      CrossCode │

      is_tainted: false
      cost: (0 0 0)
      - : unit = ()
      ]}

      [cost] is especially useful in combination with
      {{!page-index.factory}a custom cost factory}.
      See the section for further details. *)

  (** {2 Filler documents} *)

  val two_columns : (doc * doc) list -> doc
  (** ({b experimental}) [two_columns ds] is a document that lays out
      the documents in [ds] in two columns by inserting spaces between
      the two columns in each row.

      Note that this is {b not quite} a table layout, because:
      {ul {- In each row, the right column will start at the same line as
             the last line of the left column.
             The [print_doc_let_nl 33] example below demonstrates this.}
          {- The restriction to not exceed the column separator only applies to
             the last line of the left column.
             The prior lines can freely exceed the column separator.
             The [print_doc_let_nl 33] example below demonstrates this.}
          {- The right column's non-first lines can flow back to
             the beginning of the left column (use {{!Printer.Make.align}[align]}
             to restrict the right column content from flowing back).
             The [print_doc_match 54] example below demonstrates this.}}

      Also note that some rows {i may} overflow the
      column separator anyway (e.g. in order to avoid the global overflow over
      the page width limit).

      The cost of the overflow over the column separator can be adjusted via
      {{!Signature.CostFactory.two_columns_overflow}[CostFactory.two_columns_overflow]}
      (this particularly can be used to strongly discourage overflow over
      the column separator).
      The cost to prefer the leftmost column separator can be adjusted via
      {{!Signature.CostFactory.two_columns_bias}[CostFactory.two_columns_bias]}.
      Meanwhile, the cost for the inserted spaces between the two columns
      is always fixed to [CostFactory.text 0 0].

      The indentation level is set to the initial current column position
      (in the same manner as {{!Printer.Make.align}[align]}) so that on entering
      a new line, the left column of the next row starts at the right position.

      Unlike {{:https://hackage.haskell.org/package/wl-pprint-1.2.1/docs/Text-PrettyPrint-Leijen.html#v:fill}[fill]}
      or {{:https://hackage.haskell.org/package/wl-pprint-1.2.1/docs/Text-PrettyPrint-Leijen.html#v:fillBreak}[fillBreak]}
      from Wadler/Leijen's pretty printer, which requires users to specify a fixed position of the column separator,
      [two_columns] will find the position automatically, and will find a "fitting" one.

      This document is expensive. Internally, it expands to a very large document.
      Don't use it if the input list is long!

      {5 Examples:}

      The [types] example is taken from
      {{:https://hackage.haskell.org/package/wl-pprint-1.2.1/docs/Text-PrettyPrint-Leijen.html#v:fill}}

      {[
      # let types = [ "empty",     "Doc";
                      "nest",      "Int -> Doc -> Doc";
                      "linebreak", "Doc" ];;
      val types : (string * string) list =
        [("empty", "Doc"); ("nest", "Int -> Doc -> Doc"); ("linebreak", "Doc")]
      ]}

      {[
      # let print_doc_let w =
          let cf = Printer.default_cost_factory ~page_width:w () in
          let module P = Printer.Make (val cf) in
          let open P in
          let d = text "let " ^^
                  two_columns (List.map
                                 (fun (n, t) -> text n, text " :: " ^^ text t)
                                 types) in
          pretty_format_debug d |> print_endline;;
      val print_doc_let : int -> unit = <fun>
      ]}

      {[
      # print_doc_let 34;;
      1234567890123456789012345678901234
      let empty     :: Doc              │
          nest      :: Int -> Doc -> Doc│
          linebreak :: Doc              │

      is_tainted: false
      cost: (0 0 2)
      - : unit = ()
      ]}

      {[
      # print_doc_let 33;;
      123456789012345678901234567890123
      let empty :: Doc                 │
          nest  :: Int -> Doc -> Doc   │
          linebreak :: Doc             │

      is_tainted: false
      cost: (0 4 2)
      - : unit = ()
      ]}

      {[
      # print_doc_let 28;;
      1234567890123456789012345678
      let empty :: Doc            │
          nest :: Int -> Doc -> Do│c
          linebreak :: Doc        │

      is_tainted: false
      cost: (1 6 2)
      - : unit = ()
      ]}

      {[
      # let print_doc_let_nl w =
          let cf = Printer.default_cost_factory ~page_width:w () in
          let module P = Printer.Make (val cf) in
          let open P in
          let d = text "let " ^^
                  two_columns (List.map
                                 (fun (n, t) ->
                                    text n ^^ group break,
                                    text " :: " ^^ text t)
                                 types) in
          pretty_format_debug d |> print_endline;;
      val print_doc_let_nl : int -> unit = <fun>
      ]}

      {[
      # print_doc_let_nl 34;;
      1234567890123456789012345678901234
      let empty     :: Doc              │
          nest      :: Int -> Doc -> Doc│
          linebreak :: Doc              │

      is_tainted: false
      cost: (0 0 2)
      - : unit = ()
      ]}

      {[
      # print_doc_let_nl 33;;
      123456789012345678901234567890123
      let empty :: Doc                 │
          nest  :: Int -> Doc -> Doc   │
          linebreak                    │
                :: Doc                 │

      is_tainted: false
      cost: (0 0 3)
      - : unit = ()
      ]}

      {[
      # print_doc_let_nl 28;;
      1234567890123456789012345678
      let empty                   │
           :: Doc                 │
          nest                    │
           :: Int -> Doc -> Doc   │
          linebreak               │
           :: Doc                 │

      is_tainted: false
      cost: (0 0 5)
      - : unit = ()
      ]}

      {[
      # let table = [ "[]",                        "false";
                      "hd :: _ when hd = to_find", "true";
                      "_ :: tl",                   "find_member to_find tl" ];;
      val table : (string * string) list =
        [("[]", "false"); ("hd :: _ when hd = to_find", "true");
         ("_ :: tl", "find_member to_find tl")]
      ]}

      {[
      # let print_doc_match w =
          let cf = Printer.default_cost_factory ~page_width:w () in
          let module P = Printer.Make (val cf) in
          let open P in
          let d = text "let rec find_member to_find xs =" ^^
                  nest 2 (
                    nl ^^ text "match xs with" ^^ nl ^^
                    two_columns
                      (List.map
                         (fun (n, t) ->
                           (text "| " ^^ text n,
                             text " ->" ^^ group (nest 2 nl) ^^
                             text t))
                         table)) in
          pretty_format_debug d |> print_endline;;
      val print_doc_match : int -> unit = <fun>
      ]}

      {[
      # print_doc_match 55;;
      1234567890123456789012345678901234567890123456789012345
      let rec find_member to_find xs =                       │
        match xs with                                        │
        | []                        -> false                 │
        | hd :: _ when hd = to_find -> true                  │
        | _ :: tl                   -> find_member to_find tl│

      is_tainted: false
      cost: (0 0 4)
      - : unit = ()
      ]}

      {[
      # print_doc_match 54;;
      123456789012345678901234567890123456789012345678901234
      let rec find_member to_find xs =                      │
        match xs with                                       │
        | []                        -> false                │
        | hd :: _ when hd = to_find -> true                 │
        | _ :: tl                   ->                      │
          find_member to_find tl                            │

      is_tainted: false
      cost: (0 0 5)
      - : unit = ()
      ]} *)

  (** {2 Failure document} *)

  val fail : doc
  (** A document that always fails. It interacts with {!(<|>)}:
      failing branches are pruned away.

      {5 Examples:}
      {[
      # pretty_print print_string (text "Sea of Stars" ^^ fail);;
      Exception: Failure "fails to render".
      ]}

      {[
      # pretty_print print_string ((text "Sea of Stars" ^^ fail) <|> text "Hades");;
      Hades
      - : unit = ()
      ]} *)


  (** {2 Other derived combinators} *)

  val flatten : doc -> doc
  (** [flatten d] is a document that replaces newlines and indentation spaces
      with what's specified in [newline] when rendering [d].

      {5 Examples:}
      {[
      # pretty_print
          print_string
          (flatten (text "Fire Emblem" ^^ nl ^^ text "Awakening"));;
      Fire Emblem Awakening
      - : unit = ()
      ]}

      {[
      # pretty_print
          print_string
          (flatten (text "Mario + Rabbids" ^^ break ^^ text "Kingdom Battle"));;
      Mario + RabbidsKingdom Battle
      - : unit = ()
      ]}

      {[
      # pretty_print
          print_string
          (flatten (text "XCOM 2" ^^ hard_nl ^^ text "War of the Chosen"));;
      Exception: Failure "fails to render".
      ]}

      {[
      # pretty_print
          print_string
          (flatten (text "Tactics Ogre" ^^
                    newline (Some ": ") ^^
                    text "Reborn"));;
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
