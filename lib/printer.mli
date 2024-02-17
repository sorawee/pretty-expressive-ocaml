(** This module provides a pretty expressive printer. *)

module Make(C : Signature.CostFactory): (Signature.PrinterT with type cost = C.t)
(** The pretty printer and document combinators, parameterized by a cost factory. *)

module MakeCompat(C : Signature.CostFactory): (Signature.PrinterCompatT with type cost = C.t)
(** This functor is similar to {!Make}, but it provides operators
    that are compatible with the paper.
    {b Using [open] on it will shadow built-in identifiers.} *)

val make_debug_format : int -> string -> bool -> string -> string
(** [make_debug_format limit content is_tainted cost] returns a debugging string
    containing these parameters. *)

val default_cost_factory : page_width:int -> ?computation_width:int -> unit ->
                           (module Signature.CostFactory with type t = int * int * int)
(** The default cost factory, parameterized by the page width limit [page_width],
    and optionally {{!page-index.complimit}the computation width limit}
    [computation_width].
    When the computation width limit is not specified, it is set to
    [1.2 * page_width].

    In this cost factory, the cost type [t] is a quadruple of natural numbers.

    {ul {- The first component is {i badness}, which is roughly speaking
           the sum of squared overflows over the page width limit}
        {- The second component is sum of overflows over a column separator.}
        {- The third component is the height (number of newlines).}}

    Internally, [default_cost_factory] is defined as:

    {@ocaml file=printer.ml,part=default_cost_factory[
let default_cost_factory ~page_width ?computation_width () =
  (module struct
    type t = int * int * int

    let limit = match computation_width with
      | None -> (float_of_int page_width) *. 1.2 |> int_of_float
      | Some computation_width -> computation_width

    let text pos len =
      let stop = pos + len in
      if stop > page_width then
        let maxwc = max page_width pos in
        let a = maxwc - page_width in
        let b = stop - maxwc in
        b * (2*a + b), 0, 0
      else
        0, 0, 0

    let newline _ = 0, 0, 1

    let combine (o1, ot1, h1) (o2, ot2, h2) =
      o1 + o2, ot1 + ot2, h1 + h2

    let le c1 c2 = c1 <= c2

    let two_columns_overflow w = 0, w, 0

    let two_columns_bias _ = 0, 0, 0

    let string_of_cost (o, ot, h) = Printf.sprintf "(%d %d %d)" o ot h

    let debug_format = make_debug_format page_width
  end: Signature.CostFactory with type t = int * int * int)
]} *)

val version : string
(* a version string *)
