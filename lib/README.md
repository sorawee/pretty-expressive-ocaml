The organization here is a little bit weird. This note explains why.

The core of the pretty printer is `printer.ml`, and its companion signature file `printer.mli` depends on it.

In both `printer.ml` and `printer.mli`, we need to specify module types (e.g., `PrinterT`). 
However, if we define `PrinterT` in `printer.ml`, then we also need to define it again in `printer.mli`.

Why don't want to duplicate the module types over and over again. 
My solution is to define the module types in a new file `signature.mli`, 
and then both `printer.ml` and `printer.mli` can refer to it.
Note that `signature` is specified as `modules_without_implementation` under `dune`, and things work out.

Stuff that are depended on by `signature.mli` and `printer.ml` are defined in `util.ml`.
