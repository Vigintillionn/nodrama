open Isa

let memory_size = 10_000

module Symbols = Map.Make (String)

type t = { words : int array; size : int; symbols : (string * int) list }

let ( let* ) = Result.bind
let ( let+ ) result f = Result.map f result
let text_size s = (String.length s / 3) + 1
let item_size : Ast.item -> int = function Word _ -> 1 | Text s -> text_size s

let size_of : Ast.statement -> int = function
  | Instr _ -> 1
  | Data items ->
      List.fold_left (fun total item -> total + item_size item) 0 items
  | Resgr n -> n
  | Eindpr -> 0

type layout = {
  next : int;
  symbols : (int * Span.t) Symbols.t;
  placed : (int * Ast.statement) list;
  errors : Diagnostic.t list;
}

let line_number span =
  fst (Source.line_col (Span.source span) (Span.start span))

let define layout (label : Ast.ident) =
  match Symbols.find_opt label.name layout.symbols with
  | Some (_, first) ->
      let error =
        Diagnostic.error ~span:label.span
          ~notes:
            [ Printf.sprintf "first defined on line %d" (line_number first) ]
          "label `%s` is defined twice" label.name
      in
      { layout with errors = error :: layout.errors }
  | None ->
      let symbols =
        Symbols.add label.name (layout.next, label.span) layout.symbols
      in
      { layout with symbols }

let place layout span statement =
  let next = layout.next + size_of statement in
  let errors =
    if layout.next <= memory_size && next > memory_size then
      Diagnostic.error ~span
        ~notes:
          [
            Printf.sprintf "memory has %d words, this line ends at address %d"
              memory_size (next - 1);
          ]
        "program does not fit in memory"
      :: layout.errors
    else layout.errors
  in
  let placed = (layout.next, statement) :: layout.placed in
  { layout with next; placed; errors }

let layout lines =
  let empty = { next = 0; symbols = Symbols.empty; placed = []; errors = [] } in
  List.fold_left
    (fun layout (line : Ast.line) ->
      let layout = Option.fold ~none:layout ~some:(define layout) line.label in
      Option.fold ~none:layout ~some:(place layout line.span) line.statement)
    empty lines

let resolve symbols ({ const; symbol; _ } : Ast.expr) =
  match symbol with
  | None -> Ok const
  | Some (sign, label) -> (
      match Symbols.find_opt label.name symbols with
      | None ->
          Error
            (Diagnostic.error ~span:label.span "unknown label `%s`" label.name)
      | Some (address, _) -> (
          match sign with
          | Add -> Ok (const + address)
          | Sub -> Ok (const - address)))

type field = Signed | Unsigned

let field_of_mode : Mode.t -> field = function
  | Value -> Signed
  | Address | Direct | Indirect -> Unsigned

let range field (index : Index.t) =
  match field with
  | Signed -> (-5000, 4999)
  | Unsigned -> (
      match index with
      | Unindexed -> (0, 9999)
      | Indexed _ | Pre_inc _ | Post_inc _ | Pre_dec _ | Post_dec _ ->
          (-5000, 9999))

let out_of_range ~span ~lo ~hi what value =
  Error
    (Diagnostic.error ~span
       ~label:(Printf.sprintf "must be between %d and %d" lo hi)
       "%s %d is out of range" what value)

let operand symbols field ({ n; index } : Ast.expr operand) =
  let* value = resolve symbols n in
  let lo, hi = range field index in
  if value < lo || value > hi then
    out_of_range ~span:n.span ~lo ~hi "operand" value
  else Ok { n = (if value < 0 then value + 10_000 else value); index }

let instr symbols : Ast.expr instr -> (int instr, Diagnostic.t) result =
  let value mode o = operand symbols (field_of_mode mode) o in
  let target o = operand symbols Unsigned o in
  function
  | HIA (r, mode, o) ->
      let+ o = value mode o in
      HIA (r, mode, o)
  | OPT (r, mode, o) ->
      let+ o = value mode o in
      OPT (r, mode, o)
  | AFT (r, mode, o) ->
      let+ o = value mode o in
      AFT (r, mode, o)
  | VER (r, mode, o) ->
      let+ o = value mode o in
      VER (r, mode, o)
  | DEL (r, mode, o) ->
      let+ o = value mode o in
      DEL (r, mode, o)
  | MOD (r, mode, o) ->
      let+ o = value mode o in
      MOD (r, mode, o)
  | VGL (r, mode, o) ->
      let+ o = value mode o in
      VGL (r, mode, o)
  | BIG (r, t, o) ->
      let+ o = target o in
      BIG (r, t, o)
  | SPR (t, o) ->
      let+ o = target o in
      SPR (t, o)
  | VSP (cond, t, o) ->
      let+ o = target o in
      VSP (cond, t, o)
  | SBR (t, o) ->
      let+ o = target o in
      SBR (t, o)
  | KTG -> Ok KTG
  | LEZ -> Ok LEZ
  | DRU -> Ok DRU
  | DRS -> Ok DRS
  | NWL -> Ok NWL
  | NTS -> Ok NTS
  | STP -> Ok STP

let data_word symbols (e : Ast.expr) =
  let lo = -5_000_000_000 and hi = 9_999_999_999 in
  let* value = resolve symbols e in
  if value < lo || value > hi then
    out_of_range ~span:e.span ~lo ~hi "data word" value
  else Ok (if value < 0 then value + 10_000_000_000 else value)

let text_words s =
  let code i = if i < String.length s then Char.code s.[i] else 0 in
  List.init (text_size s) (fun k ->
      code (3 * k)
      + (1000 * code ((3 * k) + 1))
      + (1_000_000 * code ((3 * k) + 2)))

let item symbols : Ast.item -> (int list, Diagnostic.t) result = function
  | Word e ->
      let+ word = data_word symbols e in
      [ word ]
  | Text s -> Ok (text_words s)

let rec sequence = function
  | [] -> Ok []
  | result :: rest ->
      let* x = result in
      let+ xs = sequence rest in
      x :: xs

let words symbols : Ast.statement -> (int list, Diagnostic.t) result = function
  | Instr i ->
      let+ i = instr symbols i in
      [ Encoding.encode i ]
  | Data items ->
      let+ words = List.map (item symbols) items |> sequence in
      List.concat words
  | Resgr _ | Eindpr -> Ok []

let by_position (a : Diagnostic.t) (b : Diagnostic.t) =
  Option.compare Int.compare
    (Option.map Span.start a.span)
    (Option.map Span.start b.span)

let build layout blocks =
  let words = Array.make memory_size 0 in
  List.iter
    (fun (address, block) ->
      List.iteri (fun i word -> words.(address + i) <- word) block)
    blocks;
  let symbols =
    Symbols.bindings layout.symbols
    |> List.map (fun (name, (address, _)) -> (name, address))
  in
  { words; size = layout.next; symbols }

let assemble lines =
  let layout = layout lines in
  let blocks, errors =
    List.partition_map
      (fun (address, statement) ->
        match words layout.symbols statement with
        | Ok block -> Either.Left (address, block)
        | Error e -> Either.Right e)
      layout.placed
  in
  match List.stable_sort by_position (layout.errors @ errors) with
  | _ :: _ as errors -> Error errors
  | [] when layout.next = 0 ->
      Error
        [
          Diagnostic.error
            ~notes:[ "a program must produce at least one word" ]
            "program is empty";
        ]
  | [] -> Ok (build layout blocks)

let size (t : t) = t.size
let word (t : t) address = t.words.(address)
let symbols (t : t) = t.symbols
