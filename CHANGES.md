## 0.5 (2024-03-13)

* Improve performance in `two_columns` via the zipper data structure 
  and caching.
* Fix a bug caused from a wrong renderer is used due to memoization.
  Thanks to @zbyrn who reported the issue (#2)!

## 0.4 (2024-02-14)

* Fix a critical issue in `two_columns`: remove phantom spaces, 
  and adjust costs to ensure optimality.
* Mark `two_columns` as experimental.

## 0.3 (2024-02-11)

* Add the `two_columns` construct
* Rename `CostFacory.debug` to `CostFactory.string_of_cost`
* Reorganize pretty printing functions:
  * Rename `Printer.Make.print` to `Printer.Make.pretty_format_info` (with slightly different signature)
  * Rename `Printer.Make.pretty_print` to `Printer.Make.pretty_format`
  * Add `Printer.Make.pretty_print`, `Printer.Make.pretty_print_info`, and `Printer.Make.pretty_format_debug`.
  * Essentially, `Printer.Make.pretty_print(*)` is suitable for printing to a channel. 
    `Printer.Make.pretty_format(*)` is suitable for printing as a string. 
    The suffix `_info` indicates that it returns debugging information.

## 0.2 (2023-11-28)

* Change `<>` to `^^` to avoid shadowing the not equal operator 
  (thanks to @EmileTrotignon who made the suggestion in 
  [the OCaml forum](https://discuss.ocaml.org/t/ann-first-release-of-pretty-expressive-a-pretty-expressive-printer/13516/2)).
* Improve documentation
* Use mdx for documentation

## 0.1 (2023-11-26)

* Initial release
