type kind =
  | Ident of string
  | Int of int
  | String of string
  | Comma
  | Semicolon
  | Colon
  | LParen
  | RParen
  | Plus
  | Minus
  | Star

type token = { kind : kind; span : Span.t }
type line = { tokens : token list; eol : Span.t }
type input = { source : Source.t; text : string; stop : int }

let ( let* ) = Result.bind
let span input start stop = Span.make input.source ~start ~stop

let error input start stop fmt =
  Format.kasprintf
    (fun message ->
      Error (Diagnostic.error ~span:(span input start stop) "%s" message))
    fmt

let is_digit = function '0' .. '9' -> true | _ -> false
let is_octal = function '0' .. '7' -> true | _ -> false

let is_hex = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false

let is_ident = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' -> true
  | _ -> false

let peek input i = if i < input.stop then Some input.text.[i] else None

let rec skip ?(limit = max_int) p input i =
  match peek input i with
  | Some c when limit > 0 && p c -> skip ~limit:(limit - 1) p input (i + 1)
  | Some _ | None -> i

let slice input start stop = String.sub input.text start (stop - start)

let uchar input i =
  let d = String.get_utf_8_uchar input.text i in
  let stop = i + Uchar.utf_decode_length d in
  if Uchar.utf_decode_is_valid d then Ok (Uchar.utf_decode_uchar d, stop)
  else error input i stop "invalid UTF-8"

let number input start =
  let stop = skip is_digit input start in
  let suffix = skip is_ident input stop in
  if suffix > stop then
    error input stop suffix "invalid digit `%s` in number"
      (slice input stop suffix)
  else
    match int_of_string_opt (slice input start stop) with
    | Some n -> Ok (Int n, stop)
    | None -> error input start stop "number is too large"

let ident input start =
  let stop = skip is_ident input start in
  (Ident (slice input start stop), stop)

let escape_special_char = function
  | 'n' -> Some '\n'
  | 't' -> Some '\t'
  | 'v' -> Some '\011'
  | 'b' -> Some '\b'
  | 'r' -> Some '\r'
  | 'f' -> Some '\012'
  | 'a' -> Some '\007'
  | ('\\' | '\'' | '"' | '?') as c -> Some c
  | _ -> None

let char_code input start stop = function
  | Some n when 0 <= n && n < 256 -> Ok (Char.chr n, stop)
  | Some _ | None -> error input start stop "character code must be below 256"

let escape input i = function
  | 'x' ->
      let stop = skip is_hex input (i + 2) in
      if stop = i + 2 then
        error input i stop "`\\x` needs at least one hex digit"
      else
        char_code input i stop
          (int_of_string_opt ("0x" ^ slice input (i + 2) stop))
  | c when is_octal c ->
      let stop = skip ~limit:3 is_octal input (i + 1) in
      char_code input i stop
        (int_of_string_opt ("0o" ^ slice input (i + 1) stop))
  | c -> (
      match escape_special_char c with
      | Some c -> Ok (c, i + 2)
      | None -> error input i (i + 2) "unknown escape sequence")

let plain input i =
  let* u, stop = uchar input i in
  let code = Uchar.to_int u in
  if code < 256 then Ok (Char.chr code, stop)
  else
    error input i stop "character `%s` is not in DRAMA's 8-bit character set"
      (slice input i stop)

let string input start =
  let buf = Buffer.create 16 in
  let rec go i =
    match (peek input i, peek input (i + 1)) with
    | None, _ | Some '\\', None ->
        error input start input.stop "unterminated string literal"
    | Some '"', _ -> Ok (String (Buffer.contents buf), i + 1)
    | Some '\\', Some e -> add (escape input i e)
    | Some _, _ -> add (plain input i)
  and add result =
    let* c, next = result in
    Buffer.add_char buf c;
    go next
  in
  go (start + 1)

let unexpected input i =
  let* u, stop = uchar input i in
  error input i stop "unexpected character `%s` (U+%04X)" (slice input i stop)
    (Uchar.to_int u)

let token input i = function
  | ',' -> Ok (Comma, i + 1)
  | ';' -> Ok (Semicolon, i + 1)
  | ':' -> Ok (Colon, i + 1)
  | '(' -> Ok (LParen, i + 1)
  | ')' -> Ok (RParen, i + 1)
  | '+' -> Ok (Plus, i + 1)
  | '-' -> Ok (Minus, i + 1)
  | '*' -> Ok (Star, i + 1)
  | '"' -> string input i
  | '0' .. '9' -> number input i
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> Ok (ident input i)
  | _ -> unexpected input i

let line input start =
  let rec go i tokens =
    match peek input i with
    | None | Some '|' -> Ok { tokens = List.rev tokens; eol = span input i i }
    | Some (' ' | '\t' | '\r') -> go (i + 1) tokens
    | Some c ->
        let* kind, stop = token input i c in
        go stop ({ kind; span = span input i stop } :: tokens)
  in
  go start []

let lines source =
  let text = Source.contents source in
  let length = String.length text in
  Seq.unfold
    (fun start ->
      if start > length then None
      else
        let stop =
          String.index_from_opt text start '\n' |> Option.value ~default:length
        in
        Some (line { source; text; stop } start, stop + 1))
    0

let pp_kind ppf = function
  | Ident name -> Format.fprintf ppf "identifier `%s`" name
  | Int n -> Format.fprintf ppf "number `%d`" n
  | String s -> Format.fprintf ppf "string `%S`" s
  | Comma -> Format.fprintf ppf "`,`"
  | Semicolon -> Format.fprintf ppf "`;`"
  | Colon -> Format.fprintf ppf "`:`"
  | LParen -> Format.fprintf ppf "`(`"
  | RParen -> Format.fprintf ppf "`)`"
  | Plus -> Format.fprintf ppf "`+`"
  | Minus -> Format.fprintf ppf "`-`"
  | Star -> Format.fprintf ppf "`*`"
