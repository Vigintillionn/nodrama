open Nodrama
open Cmdliner

let ( let* ) = Result.bind

type stage = Tokens | Ast | Image | Asm | Exe
type destination = Stdout | File of string

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

let write_asm destination image =
  let emit ppf = Format.fprintf ppf "%a@?" Emit.program image in
  match destination with
  | Stdout -> Ok (emit Format.std_formatter)
  | File path -> (
      match Out_channel.open_text path with
      | exception Sys_error reason ->
          Error [ Diagnostic.error "cannot write %s: %s" path reason ]
      | oc ->
          Fun.protect
            ~finally:(fun () -> Out_channel.close oc)
            (fun () -> Ok (emit (Format.formatter_of_out_channel oc))))

let destination ~ext ~input = function
  | Some "-" -> Ok Stdout
  | Some path -> Ok (File path)
  | None -> (
      match
        Filename.chop_suffix_opt ~suffix:".dra" (Filename.basename input)
      with
      | Some base -> Ok (File (base ^ ext))
      | None ->
          Error
            [
              Diagnostic.error
                ~notes:[ "name the output with -o" ]
                "cannot derive an output name from %s" input;
            ])

let assemble source =
  let* lines = Lexer.lines source |> Parser.parse in
  Assembler.assemble lines

let compile ~stage ~output ~cc input =
  let* source = read_source input in
  match stage with
  | Tokens -> print_tokens source
  | Ast -> Lexer.lines source |> Parser.parse |> Result.map print_ast
  | Image -> assemble source |> Result.map print_image
  | Asm ->
      let* destination = destination ~ext:".s" ~input output in
      let* image = assemble source in
      write_asm destination image
  | Exe -> (
      let* destination = destination ~ext:"" ~input output in
      let* image = assemble source in
      match destination with
      | Stdout ->
          Error
            [ Diagnostic.error "cannot write an executable to standard output" ]
      | File path ->
          Link.executable ~cc ~output:path (fun ppf -> Emit.program ppf image))

let exit_diagnostics = 1

let run stage output cc input =
  match compile ~stage ~output ~cc input with
  | Ok () -> Cmd.Exit.ok
  | Error diagnostics ->
      List.iter (Format.eprintf "%a@." Diagnostic.pp) diagnostics;
      exit_diagnostics

let stage =
  let stop_after name doc = Arg.info [ name ] ~doc in
  Arg.(
    value
    & vflag Exe
        [
          (Tokens, stop_after "tokens" "Print the tokens of each line and stop.");
          (Ast, stop_after "ast" "Print the parsed program and stop.");
          ( Image,
            stop_after "image" "Print the assembled memory image and stop." );
          (Asm, stop_after "S" "Write RISC-V assembly instead of an executable.");
        ])

let output =
  let doc =
    "Write the output to $(docv). With $(b,-S), $(b,-) means standard output. \
     The default is the input's name without $(b,.dra), plus $(b,.s) with \
     $(b,-S), in the current directory."
  in
  Arg.(value & opt (some filepath) None & info [ "o" ] ~docv:"FILE" ~doc)

let cc =
  let doc = "The RISC-V Linux cross compiler used to link the executable." in
  let env = Cmd.Env.info "NODRAMA_CC" in
  Arg.(
    value
    & opt string "riscv64-linux-gnu-gcc"
    & info [ "cc" ] ~env ~docv:"CMD" ~doc)

let input =
  let doc = "The DRAMA source file." in
  Arg.(required & pos 0 (some filepath) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "compile DRAMA to a RISC-V Linux executable" in
  let exits =
    Cmd.Exit.info exit_diagnostics ~doc:"on compilation errors."
    :: Cmd.Exit.defaults
  in
  Cmd.make
    (Cmd.info "nodrama" ~doc ~exits)
    Term.(const run $ stage $ output $ cc $ input)

let () = exit (Cmd.eval' cmd)
