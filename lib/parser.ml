open Isa
open Ast

let ( let* ) = Result.bind

let error span fmt =
  Format.kasprintf
    (fun message -> Error (Diagnostic.error ~span "%s" message))
    fmt

let expected ~eol what = function
  | [] -> error eol "expected %s, found end of line" what
  | (token : Lexer.token) :: _ ->
      error token.span "expected %s, found %a" what Lexer.pp_kind token.kind

let finish ~eol value = function
  | [] -> Ok value
  | tokens -> expected ~eol "end of line" tokens

let comma ~eol : Lexer.token list -> _ = function
  | { kind = Comma; _ } :: rest -> Ok rest
  | tokens -> expected ~eol "`,`" tokens

let number ~eol : Lexer.token list -> _ = function
  | { kind = Int n; span } :: rest -> Ok (n, span, rest)
  | tokens -> expected ~eol "a number" tokens

let ident ~eol : Lexer.token list -> _ = function
  | { kind = Ident name; span } :: rest ->
      Ok ({ name = String.uppercase_ascii name; span }, rest)
  | tokens -> expected ~eol "an identifier" tokens

let register_of_name name =
  match String.uppercase_ascii name with
  | "SR" -> Some Reg.R9
  | s when String.length s = 2 && s.[0] = 'R' ->
      Option.bind (int_of_string_opt (String.sub s 1 1)) Reg.of_int
  | _ -> None

let register ~eol : Lexer.token list -> _ = function
  | { kind = Ident name; _ } :: rest as tokens -> (
      match register_of_name name with
      | Some reg -> Ok (reg, rest)
      | None -> expected ~eol "a register" tokens)
  | tokens -> expected ~eol "a register" tokens

let cond_of_name name =
  match String.uppercase_ascii name with
  | "NUL" | "GEL" -> Some Cond.NUL
  | "NNUL" | "NGEL" -> Some Cond.NNUL
  | "POS" | "GR" -> Some Cond.POS
  | "NEG" | "KL" -> Some Cond.NEG
  | "NNEG" | "GRG" -> Some Cond.NNEG
  | "NPOS" | "KLG" -> Some Cond.NPOS
  | "OVL" -> Some Cond.OVL
  | "GOVL" -> Some Cond.GOVL
  | "SO" -> Some Cond.SO
  | "GSO" -> Some Cond.GSO
  | _ -> None

let sign : Lexer.token list -> _ = function
  | { kind = Plus; span } :: rest -> Some (Add, span, rest)
  | { kind = Minus; span } :: rest -> Some (Sub, span, rest)
  | _ -> None

let apply op n = match op with Add -> n | Sub -> -n

let expr ~eol tokens =
  let ok const symbol first last rest =
    Ok ({ const; symbol; span = Span.merge first last }, rest)
  in
  match (tokens : Lexer.token list) with
  | { kind = Ident name; span } :: rest -> (
      let sym = Some (Add, { name = String.uppercase_ascii name; span }) in
      match sign rest with
      | Some (op, _, rest) ->
          let* n, stop, rest = number ~eol rest in
          ok (apply op n) sym span stop rest
      | None -> ok 0 sym span span rest)
  | { kind = Int n; span } :: rest -> (
      match sign rest with
      | Some (op, _, rest) ->
          let* s, rest = ident ~eol rest in
          ok n (Some (op, s)) span s.span rest
      | None -> ok n None span span rest)
  | tokens -> (
      match sign tokens with
      | Some (op, start, rest) ->
          let* n, stop, rest = number ~eol rest in
          ok (apply op n) None start stop rest
      | None -> expected ~eol "an expression" tokens)

let index ~eol tokens =
  let close index : Lexer.token list -> _ = function
    | { kind = RParen; _ } :: rest -> Ok (index, rest)
    | tokens -> expected ~eol "`)`" tokens
  in
  match (tokens : Lexer.token list) with
  | { kind = LParen; _ } :: { kind = Plus; _ } :: rest ->
      let* r, rest = register ~eol rest in
      close (Index.Pre_inc r) rest
  | { kind = LParen; _ } :: { kind = Minus; _ } :: rest ->
      let* r, rest = register ~eol rest in
      close (Index.Pre_dec r) rest
  | { kind = LParen; _ } :: rest -> (
      let* r, rest = register ~eol rest in
      match rest with
      | { kind = Plus; _ } :: rest -> close (Index.Post_inc r) rest
      | { kind = Minus; _ } :: rest -> close (Index.Post_dec r) rest
      | rest -> close (Index.Indexed r) rest)
  | rest -> Ok (Index.Unindexed, rest)

let address ~eol tokens =
  let* n, rest = expr ~eol tokens in
  let* index, rest = index ~eol rest in
  Ok ({ n; index }, rest)

let zero span = { const = 0; symbol = None; span }

let memory_operand ~eol tokens =
  let* operand, rest = address ~eol tokens in
  finish ~eol (Mode.Direct, operand) rest

let source_operand ~eol tokens =
  match (tokens : Lexer.token list) with
  | [ { kind = Ident name; span } ] -> (
      match register_of_name name with
      | Some r -> Ok (Mode.Value, { n = zero span; index = Indexed r })
      | None -> memory_operand ~eol tokens)
  | tokens -> memory_operand ~eol tokens

let condition : Lexer.token list -> _ = function
  | { kind = Ident name; span } :: { kind = Comma; _ } :: rest -> (
      match cond_of_name name with
      | Some cond -> Ok (cond, rest)
      | None -> error span "unknown condition `%s`" name)
  | rest -> Ok (Cond.NNEG, rest)

type shape =
  | Class1 of (Reg.t -> Mode.t -> expr operand -> expr instr)
  | Store
  | Jump of (Target.t -> expr operand -> expr instr)
  | Branch
  | Pop
  | Push
  | Bare of expr instr
  | Unsupported

let shape_of_mnemonic = function
  | "HIA" -> Some (Class1 (fun r m o -> HIA (r, m, o)))
  | "OPT" -> Some (Class1 (fun r m o -> OPT (r, m, o)))
  | "AFT" -> Some (Class1 (fun r m o -> AFT (r, m, o)))
  | "VER" -> Some (Class1 (fun r m o -> VER (r, m, o)))
  | "DEL" -> Some (Class1 (fun r m o -> DEL (r, m, o)))
  | "MOD" -> Some (Class1 (fun r m o -> MOD (r, m, o)))
  | "VGL" -> Some (Class1 (fun r m o -> VGL (r, m, o)))
  | "BIG" -> Some Store
  | "SPR" -> Some (Jump (fun t o -> SPR (t, o)))
  | "SBR" -> Some (Jump (fun t o -> SBR (t, o)))
  | "VSP" -> Some Branch
  | "HST" -> Some Pop
  | "BST" -> Some Push
  | "KTG" -> Some (Bare KTG)
  | "LEZ" -> Some (Bare LEZ)
  | "DRU" -> Some (Bare DRU)
  | "DRS" -> Some (Bare DRS)
  | "NWL" -> Some (Bare NWL)
  | "NTS" -> Some (Bare NTS)
  | "STP" -> Some (Bare STP)
  | "OND" | "KTO" | "INV" | "UTV" | "HIB" | "SGI" | "SGU" | "MKL" | "MKH"
  | "TSM" | "TSO" ->
      Some Unsupported
  | _ -> None

let mode_of_suffix ~span ~word mnemonic = function
  | "" -> Ok None
  | ".W" -> Ok (Some Mode.Value)
  | ".A" -> Ok (Some Mode.Address)
  | ".D" -> Ok (Some Mode.Direct)
  | ".I" -> Ok (Some Mode.Indirect)
  | _ ->
      Error
        (Diagnostic.error ~span
           ~notes:
             [
               Printf.sprintf
                 "the first three letters are read as the mnemonic `%s`"
                 mnemonic;
             ]
           "unknown instruction `%s`" word)

let target ~span mnemonic : Mode.t option -> _ = function
  | None | Some Direct -> Ok Target.Direct
  | Some Indirect -> Ok Target.Indirect
  | Some (Value | Address) -> error span "`%s` only takes `.d` or `.i`" mnemonic

let no_mode ~span mnemonic : Mode.t option -> _ = function
  | None -> Ok ()
  | Some _ -> error span "`%s` takes no mode" mnemonic

let instruction ~eol ~span ~word shape tokens =
  let upper = String.uppercase_ascii word in
  let mnemonic = String.sub upper 0 3 in
  let* mode =
    mode_of_suffix ~span ~word mnemonic
      (String.sub upper 3 (String.length upper - 3))
  in
  match shape with
  | Class1 make ->
      let* r, rest = register ~eol tokens in
      let* rest = comma ~eol rest in
      let* default, operand = source_operand ~eol rest in
      Ok (make r (Option.value mode ~default) operand)
  | Store ->
      let* target = target ~span mnemonic mode in
      let* r, rest = register ~eol tokens in
      let* rest = comma ~eol rest in
      let* operand, rest = address ~eol rest in
      finish ~eol (BIG (r, target, operand)) rest
  | Jump make ->
      let* target = target ~span mnemonic mode in
      let* operand, rest = address ~eol tokens in
      finish ~eol (make target operand) rest
  | Branch ->
      let* target = target ~span mnemonic mode in
      let* cond, rest = condition tokens in
      let* operand, rest = address ~eol rest in
      finish ~eol (VSP (cond, target, operand)) rest
  | Pop ->
      let* () = no_mode ~span mnemonic mode in
      let* r, rest = register ~eol tokens in
      finish ~eol (HIA (r, Direct, { n = zero span; index = Post_inc R9 })) rest
  | Push ->
      let* () = no_mode ~span mnemonic mode in
      let* r, rest = register ~eol tokens in
      finish ~eol (BIG (r, Direct, { n = zero span; index = Pre_dec R9 })) rest
  | Bare instr ->
      let* () = no_mode ~span mnemonic mode in
      finish ~eol instr tokens
  | Unsupported -> error span "`%s` is not supported" mnemonic

let operator : Lexer.kind -> _ = function
  | Plus -> Some ( + )
  | Minus -> Some ( - )
  | Star -> Some ( * )
  | Ident _ | Int _ | String _ | Comma | Semicolon | Colon | LParen | RParen ->
      None

let resgr ~eol ~span tokens =
  let rec go total : Lexer.token list -> _ = function
    | [] -> Ok total
    | { kind; _ } :: rest as tokens -> (
        match operator kind with
        | Some op ->
            let* n, _, rest = number ~eol rest in
            go (op total n) rest
        | None -> expected ~eol "`+`, `-`, or `*`" tokens)
  in
  let* n, _, rest = number ~eol tokens in
  let* count = go n rest in
  if count < 0 then error span "cannot reserve a negative number of words"
  else Ok (Resgr count)

let data ~eol tokens =
  let item : Lexer.token list -> _ = function
    | { kind = String s; _ } :: rest -> Ok (Text s, rest)
    | tokens ->
        let* e, rest = expr ~eol tokens in
        Ok (Word e, rest)
  in
  let rec go items tokens =
    let* item, rest = item tokens in
    match (rest : Lexer.token list) with
    | [] -> Ok (Data (List.rev (item :: items)))
    | { kind = Semicolon; _ } :: rest -> go (item :: items) rest
    | tokens -> expected ~eol "`;` or end of line" tokens
  in
  go [] tokens

let statement ~eol ~span tokens =
  match (tokens : Lexer.token list) with
  | { kind = Ident word; span = word_span } :: rest -> (
      let upper = String.uppercase_ascii word in
      let shape =
        if String.length upper < 3 then None
        else shape_of_mnemonic (String.sub upper 0 3)
      in
      match (upper, shape) with
      | "RESGR", _ -> resgr ~eol ~span rest
      | "EINDPR", _ -> finish ~eol Eindpr rest
      | _, Some shape ->
          let* instr = instruction ~eol ~span:word_span ~word shape rest in
          Ok (Instr instr)
      | _, None -> data ~eol tokens)
  | tokens -> data ~eol tokens

let line ({ tokens; eol } : Lexer.line) =
  let label, tokens =
    match tokens with
    | { kind = Ident name; span } :: { kind = Colon; _ } :: rest ->
        (Some { name = String.uppercase_ascii name; span }, rest)
    | tokens -> (None, tokens)
  in
  match tokens with
  | [] -> Ok { label; statement = None; span = eol }
  | first :: rest ->
      let span =
        List.fold_left
          (fun _ (token : Lexer.token) -> token.span)
          first.span rest
        |> Span.merge first.span
      in
      let* statement = statement ~eol ~span tokens in
      Ok { label; statement = Some statement; span }

let parse lines =
  let rec go parsed errors seq =
    match seq () with
    | Seq.Nil -> (parsed, errors)
    | Seq.Cons (result, rest) -> (
        match Result.bind result line with
        | Ok ({ statement = Some Eindpr; _ } as last) -> (last :: parsed, errors)
        | Ok l -> go (l :: parsed) errors rest
        | Error diagnostic -> go parsed (diagnostic :: errors) rest)
  in
  match go [] [] lines with
  | parsed, [] -> Ok (List.rev parsed)
  | _, errors -> Error (List.rev errors)
