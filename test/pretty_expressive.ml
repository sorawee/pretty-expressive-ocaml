open Pretty_expressive

let print_doc_choice (w : int) =
  let cf = Printer.default_cost_factory ~page_width:w () in
  let module P = Printer.Make (val cf) in
  let open P in

  let d = text "while (true) {" ^^
          nest 4
            (nl ^^ text "f();" ^^ nl ^^ text "if (done())" ^^
             (let exit_d = text "exit();" in
              (space ^^ exit_d) <|> nest 4 (nl ^^ exit_d))) ^^
          nl ^^ text "}"
  in
  pretty_format d

let print_doc_group (w : int) =
  let cf = Printer.default_cost_factory ~page_width:w () in
  let module P = Printer.Make (val cf) in
  let open P in

  let d = text "while (true) {" ^^
          nest 4
            (nl ^^ text "f();" ^^ nl ^^ text "if (done())" ^^
             group (nest 4 (nl ^^ text "exit();"))) ^^
          nl ^^ text "}"
  in
  pretty_format d

let test_choice_doc_80 () =
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "while (true) {" ;
         "    f();" ;
         "    if (done()) exit();" ;
         "}"
       ])
    (print_doc_choice 80)

let test_choice_doc_20 () =
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "while (true) {" ;
         "    f();" ;
         "    if (done())" ;
         "        exit();" ;
         "}"
       ])
    (print_doc_choice 20)

let test_group_doc_80 () =
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "while (true) {" ;
         "    f();" ;
         "    if (done()) exit();" ;
         "}"
       ])
    (print_doc_group 80)

let test_group_doc_20 () =
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "while (true) {" ;
         "    f();" ;
         "    if (done())" ;
         "        exit();" ;
         "}"
       ])
    (print_doc_group 20)

(******************************************************************************)

type sexp = Atom of string | List of sexp list

let print_sexp (s : sexp) (w : int) =
  let cf = Printer.default_cost_factory ~page_width:w () in
  let module P = Printer.Make (val cf) in
  let open P in

  let acat = fold_doc (fun x y -> x <+> space <+> y) in

  let rec pretty (s : sexp) =
    match s with
    | Atom s -> text s
    | List [] -> lparen <+> rparen
    | List [x] -> lparen <+> pretty x <+> rparen
    | List (x :: xs) ->
      let x_d = pretty x in
      let xs_d = List.map pretty xs in
      lparen <+>
      (acat (x_d :: xs_d) <|> (* the horizontal style *)
       vcat (x_d :: xs_d) <|> (* the vertical style *)
       (x_d <+> space <+> vcat xs_d)) <+> (* the argument list style *)
      rparen
  in
  pretty_format (pretty s)

let example_sexp = List [Atom "a"; Atom "b"; Atom "c"; Atom "d"]

let test_sexp_4 () =
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "(a" ;
         " b" ;
         " c" ;
         " d)"
       ])
    (print_sexp example_sexp 4)

let test_sexp_6 () =
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "(a b" ;
         "   c" ;
         "   d)"
       ])
    (print_sexp example_sexp 6)

let test_sexp_10 () =
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "(a b c d)" ])
    (print_sexp example_sexp 10)

let suite =
  [ "choice; w = 80", `Quick, test_choice_doc_80 ;
    "choice; w = 20", `Quick, test_choice_doc_20 ;
    "group; w = 80", `Quick, test_group_doc_80 ;
    "group; w = 20", `Quick, test_group_doc_20 ;
    "sexp; w = 4", `Quick, test_sexp_4 ;
    "sexp; w = 6", `Quick, test_sexp_6 ;
    "sexp; w = 10", `Quick, test_sexp_10
  ]

let () =
  Alcotest.run "pretty expressive" [ "example doc", suite ]
