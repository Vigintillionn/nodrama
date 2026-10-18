open Nodrama

let ( let* ) = Result.bind

type dump = Tokens | Ast | Image
type options = { input : string; dump : dump option }

let usage = "Usage: nodrama [--tokens| --ast| --image] <input>"

let parse_args () =
  let inputs = ref [] in
  let dump = ref None in
  let set value = Arg.Unit (fun () -> dump := Some value) in
  let specs =
    Arg.align
      [
        ("--tokens", set Tokens, " Print the tokens of each line and stop");
        ("--ast", set Ast, " Print the parsed program and stop");
        ("--image", set Image, " Print the assembled image and stop");
      ]
  in
  Arg.parse specs (fun path -> inputs := path :: !inputs) usage;
  match !inputs with
  | [ input ] -> Ok { input; dump = !dump }
  | [] -> Error "no input file"
  | _ -> Error "expected exactly one input file"

let read_source path =
  Source.read_file path
  |> Result.map_error (fun reason ->
      [ Diagnostic.error "cannot read %s: %s" path reason ])

let print_token source (token : Lexer.token) =
  let line, col = Source.line_col source (Span.start token.span) in
  Format.printf "%d:%d\t%a@." line col Lexer.pp_kind token.kind

let print_tokens source =
  let lines, errors =
    Lexer.lines source |> List.of_seq
    |> List.partition_map (function
      | Ok line -> Either.Left line
      | Error diagnostic -> Either.Right diagnostic)
  in
  List.iter
    (fun (line : Lexer.line) -> List.iter (print_token source) line.tokens)
    lines;
  match errors with [] -> Ok () | _ :: _ -> Error errors

let print_ast lines = List.iter (Format.printf "%a@." Ast.pp_line) lines

let print_word image address =
  let word = Assembler.word image address in
  match Encoding.decode word with
  | Some instr ->
      Format.printf "%04d  %010d  %a@." address word
        (Isa.pp Format.pp_print_int)
        instr
  | None -> Format.printf "%04d  %010d@." address word

let print_image image =
  List.iter
    (fun (name, address) -> Format.printf "%-12s %04d@." name address)
    (Assembler.symbols image);
  List.init (Assembler.size image) Fun.id |> List.iter (print_word image)

let compile { input; dump } =
  let* source = read_source input in
  match dump with
  | Some Tokens -> print_tokens source
  | Some Ast -> Lexer.lines source |> Parser.parse |> Result.map print_ast
  | Some Image ->
      let* lines = Lexer.lines source |> Parser.parse in
      Assembler.assemble lines |> Result.map print_image
  | None ->
      let* lines = Lexer.lines source |> Parser.parse in
      Assembler.assemble lines |> Result.map ignore

let () =
  match parse_args () with
  | Error message ->
      Printf.eprintf "nodrama: %s\n%s\n" message usage;
      exit 2
  | Ok options -> (
      match compile options with
      | Ok () -> exit 0
      | Error diagnostics ->
          List.iter (Format.eprintf "%a@." Diagnostic.pp) diagnostics;
          exit 1)
