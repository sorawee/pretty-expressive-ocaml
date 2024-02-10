(** This module provides a pretty expressive printer. *)

module Make(C : Signature.CostFactory): (Signature.PrinterT with type cost = C.t)
(** The pretty printer and document combinators, parameterized by a cost factory. *)

module MakeCompat(C : Signature.CostFactory): (Signature.PrinterCompatT with type cost = C.t)
(** This functor is similar to {!Make}, but it provides operators
    that are compatible with the paper.
    {b Using [open] on it will shadow built-in identifiers.} *)

val default_cost_factory : page_width:int -> ?computation_width:int -> unit ->
                           (module Signature.CostFactory with type t = int * int)
(** The default cost factory, parameterized by the page width limit [page_width],
    and optionally {{!page-index.complimit}the computation width limit}
    [computation_width].
    When the computation width limit is not specified, it is set to
    [1.2 * page_width].

    In this cost factory, the cost type [t] is a pair of natural numbers,
    where the first component is {i badness},
    which is roughly speaking the sum of squared overflows,
    and the second component is the height (number of newlines).

    Internally, [default_cost_factory] is defined as:

    {@ocaml file=printer.ml,part=default_cost_factory[
let default_cost_factory ~page_width ?computation_width () =
  (module struct
    type t = int * int
    let limit = match computation_width with
      | None -> (float_of_int page_width) *. 1.2 |> int_of_float
      | Some computation_width -> computation_width

    let text pos len =
      let stop = pos + len in
      if stop > page_width then
        let maxwc = max page_width pos in
        let a = maxwc - page_width in
        let b = stop - maxwc in
        (b * (2*a + b), 0)
      else
        (0, 0)

    let newline _ = (0, 1)

    let combine (o1, h1) (o2, h2) =
      (o1 + o2, h1 + h2)

    let le (o1, h1) (o2, h2) =
      if o1 = o2 then h1 <= h2 else o1 < o2

    let debug (o, h) = Printf.sprintf "(%d %d)" o h

  end: Signature.CostFactory with type t = int * int)
]} *)
