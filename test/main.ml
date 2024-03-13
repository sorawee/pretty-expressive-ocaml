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

let horz_layout =
  String.concat "\n"
    [ "while (true) {";
      "    f();";
      "    if (done()) exit();";
      "}" ]

let vert_layout =
  String.concat "\n"
    [ "while (true) {";
      "    f();";
      "    if (done())";
      "        exit();";
      "}" ]

let test_choice_doc_80 () =
  Alcotest.(check string) "same string" horz_layout (print_doc_choice 80)

let test_choice_doc_20 () =
  Alcotest.(check string) "same string" vert_layout (print_doc_choice 20)

let test_group_doc_80 () =
  Alcotest.(check string) "same string" horz_layout (print_doc_group 80)

let test_group_doc_20 () =
  Alcotest.(check string) "same string" vert_layout (print_doc_group 20)

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
       [ "(a";
         " b";
         " c";
         " d)" ])
    (print_sexp example_sexp 4)

let test_sexp_6 () =
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "(a b";
         "   c";
         "   d)" ])
    (print_sexp example_sexp 6)

let test_sexp_10 () =
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "(a b c d)" ])
    (print_sexp example_sexp 10)

(******************************************************************************)

let test_two_columns_case_1 () =
  let cf = Printer.default_cost_factory ~page_width:4 ~computation_width:100 () in
  let module P = Printer.Make (val cf) in
  let open P in

  let d1 = text "x" <$>
           text "xxx" in
  let d2 = text "xxx" <$>
           text "x" in
  let d_below = text "" in
  let d_right1 = text "zz" in
  let d_right2 = text "wwww" in

  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "1234";
         "xxx │" ;
         "xzz │";
         "wwww│";
         "";
         "is_tainted: false";
         "cost: (0 1 2)" ])
    (pretty_format_debug (two_columns [ d1 <|> d2, d_right1;
                                        d_below,   d_right2 ]))

let test_two_columns_case_2 () =
  let cf = Printer.default_cost_factory ~page_width:5 ~computation_width:100 () in
  let module P = Printer.Make (val cf) in
  let open P in

  let d1 = text "x" <$>
           text "xxx" in
  let d2 = text "xxx" <$>
           text "x" in
  let d_below = text "qq" in
  let d_right1 = text "zzz" in
  let d_right2 = text "www" in

  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "12345";
         "xxx  │" ;
         "x zzz│";
         "qqwww│";
         "";
         "is_tainted: false";
         "cost: (0 0 2)" ])
    (pretty_format_debug (two_columns [ d1 <|> d2, d_right1;
                                        d_below,   d_right2 ]))

let test_two_columns_case_3 () =
  let cf = Printer.default_cost_factory ~page_width:7 ~computation_width:100 () in
  let module P = Printer.Make (val cf) in
  let open P in

  let d1 = text "x" <$>
           text "xxx" in
  let d2 = text "xxx" <$>
           text "x" in
  let d_below = text "qqqq" in
  let d_right1 = text "zzz" in
  let d_right2 = text "www" in

  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "1234567";
         "xxx    │" ;
         "x   zzz│";
         "qqqqwww│";
         "";
         "is_tainted: false";
         "cost: (0 0 2)" ])
    (pretty_format_debug (two_columns [ d1 <|> d2, d_right1;
                                        d_below,   d_right2 ]))

let test_two_columns_regression_phantom () =
  let cf = Printer.default_cost_factory ~page_width:7 ~computation_width:100 () in
  let module P = Printer.Make (val cf) in
  let open P in

  (* creates column separator at 3 *)
  let phantom_doc = text "aaaaaaaaaaaaaaaa" <$>
                    text "a" <$>
                    text "aaa" in
  let d = text "a" <$>
          text "aaaa" in
  let d_below = text "q" in
  let d_right1 = text "zzz" in
  let d_right2 = text "wwww" in

  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "1234567";
         "a      │" ;
         "aaaazzz│";
         "qwwww  │";
         "";
         "is_tainted: false";
         "cost: (0 3 2)" ])
    (pretty_format_debug (two_columns [ phantom_doc <|> d, d_right1;
                                        d_below,           d_right2 ]))

(* This is a cost factory that cares more about preserving the two-column shape
   than avoiding overflows *)
let strict_two_columns_cost_factory ~page_width ?computation_width () =
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
        0, b * (2*a + b), 0
      else
        0, 0, 0

    let newline _ = 0, 0, 1

    let combine (ot1, o1, h1) (ot2, o2, h2) =
      ot1 + ot2, o1 + o2, h1 + h2

    let le c1 c2 = c1 <= c2

    let two_columns_overflow w = w, 0, 0

    let two_columns_bias _ = 0, 0, 0

    let string_of_cost (ot, o, h) = Printf.sprintf "(%d %d %d)" ot o h

    let debug_format = Printer.make_debug_format page_width
  end: Signature.CostFactory with type t = int * int * int)

let test_two_columns_factory_overflow () =
  let cf = strict_two_columns_cost_factory ~page_width:4 ~computation_width:100 () in
  let module P = Printer.Make (val cf) in
  let open P in

  let d1 = text "xxx" in
  let d2 = text "xxxxx" <$>
           text "x" in
  let d_below = text "" in
  let d_right1 = text "zz" in
  let d_right2 = text "wwww" in

  (* NOTE: choosing d2 is better than choosing d1,
     since d_right1 will overflow as much as d2,
     and d_right2 will overflow A LOT *)
  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "1234";
         "xxxx│x";
         "xzz │";
         " www│w";
         "";
         "is_tainted: false";
         "cost: (0 2 2)" ])
    (pretty_format_debug (two_columns [ d1 <|> d2, d_right1;
                                        d_below,   d_right2 ]))

(* This is a cost factory that cares about choosing leftmost separator, more than
   minimizing number of lines. It still tries to avoid overflows though *)
let biased_two_columns_cost_factory ~page_width ?computation_width () =
  (module struct
    type t = int * int * int * int

    let limit = match computation_width with
      | None -> (float_of_int page_width) *. 1.2 |> int_of_float
      | Some computation_width -> computation_width

    let text pos len =
      let stop = pos + len in
      if stop > page_width then
        let maxwc = max page_width pos in
        let a = maxwc - page_width in
        let b = stop - maxwc in
        b * (2*a + b), 0, 0, 0
      else
        0, 0, 0, 0

    let newline _ = 0, 0, 0, 1

    let combine (o1, ot1, b1, h1) (o2, ot2, b2, h2) =
      (o1 + o2, ot1 + ot2, b1 + b2, h1 + h2)

    let le c1 c2 = c1 <= c2

    let two_columns_overflow w = 0, w, 0, 0

    let two_columns_bias w = 0, 0, w, 0

    let string_of_cost (ot, o, b, h) = Printf.sprintf "(%d %d %d %d)" ot o b h

    let debug_format = Printer.make_debug_format page_width
  end: Signature.CostFactory with type t = int * int * int * int)

let test_two_columns_factory_bias () =
  let cf = biased_two_columns_cost_factory ~page_width:4 ~computation_width:100 () in
  let module P = Printer.Make (val cf) in
  let open P in

  let d1 = text "xxxxx" <$>
           text "x" <$>
           text "x" in
  let d2 = text "xx" <$>
           text "xx" in
  let d3 = text "xxx" in
  let d_below = text "" in
  let d_right1 = text "z" in
  let d_right2 = text "w" in

  Alcotest.(check string) "same string"
    (String.concat "\n"
       [ "1234";
         "xx  │" ;
         "xxz │";
         "  w │";
         "";
         "is_tainted: false";
         "cost: (0 0 2 2)" ])
    (pretty_format_debug (two_columns [ d1 <|> d2 <|> d3, d_right1;
                                        d_below,          d_right2 ]))

let test_two_columns_performance () =
  let cf = Printer.default_cost_factory ~page_width:100 ~computation_width:200 () in
  let module P = Printer.Make (val cf) in
  let open P in
  let rec make_lines (n : int): doc =
    if n = 1 then text "x"
    else text "x" <$> make_lines (n - 1)
  in
  let make_choices (k : int): doc =
    let rec loop (i : int): doc =
      let doc =
        (make_lines i) <+>
        text (String.make (k - i) 'a')
      in if i = 1 then doc else doc <|> loop (i - 1)
    in loop k
  in
  let d_left = make_choices 100 in
  let d_right = text "zzz" in
  let rec make_rows (k : int) =
    if k = 0 then
      []
    else
      (d_left, d_right) :: make_rows (k - 1)
  in
  Alcotest.(check pass) "run quickly with 10 rows in 10 different contexts"
    true
    (pretty_format_debug ((make_choices 10) ^^ (two_columns (make_rows 10)))
     |> ignore;
     true);
  Alcotest.(check pass) "run quickly with 100 rows in the same context"
    true
    (pretty_format_debug (two_columns (make_rows 100))
     |> ignore;
     true)

let test_different_renderer () =
  let w = 20 in
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
  Alcotest.(check string) "same string" vert_layout (pretty_format d);
  Alcotest.(check string) "same string" vert_layout (pretty_format d)

let () =
  Alcotest.run "pretty expressive"
    [ "example doc",
      [ "choice; w = 80", `Quick, test_choice_doc_80;
        "choice; w = 20", `Quick, test_choice_doc_20;
        "group; w = 80", `Quick, test_group_doc_80;
        "group; w = 20", `Quick, test_group_doc_20 ];

      "sexp",
      [ "sexp; w = 4", `Quick, test_sexp_4;
        "sexp; w = 6", `Quick, test_sexp_6;
        "sexp; w = 10", `Quick, test_sexp_10 ];

      "two_columns",
      [ "case 1", `Quick, test_two_columns_case_1;
        "case 2", `Quick, test_two_columns_case_2;
        "case 3", `Quick, test_two_columns_case_3;
        "regression phantom space", `Quick, test_two_columns_regression_phantom;
        "cost factory - overflow", `Quick, test_two_columns_factory_overflow;
        "cost factory - bias", `Quick, test_two_columns_factory_bias;
        "performance", `Slow, test_two_columns_performance ];

      "regression test",
      [ "different renderer (issue #2)", `Quick, test_different_renderer ] ]
