(** This module provides utilities. *)

type info = {
  out : string;
  (** Output of the pretty printer *)
  is_tainted : bool;
  (** Taintedness status *)
  cost : string
  (** Cost of the output layout *)
}
(** An [info] record, returned from the pretty printer. *)
