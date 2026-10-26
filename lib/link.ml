let ( let* ) = Result.bind
let program = "program.s"
let flags = [ "-static"; "-nostdlib"; "-ffreestanding" ]
let command_not_found = 127

let write_file path print =
  Out_channel.with_open_text path (fun oc ->
      let ppf = Format.formatter_of_out_channel oc in
      print ppf;
      Format.pp_print_flush ppf ())

let write_files dir asm =
  let write name print = write_file (Filename.concat dir name) print in
  match
    write program asm;
    List.iter
      (fun (name, contents) ->
        write name (fun ppf -> Format.pp_print_string ppf contents))
      Runtime_files.files
  with
  | () -> Ok ()
  | exception Sys_error reason ->
      Error [ Diagnostic.error "cannot write to %s: %s" dir reason ]

let remove_dir dir =
  Sys.readdir dir
  |> Array.iter (fun name -> Sys.remove (Filename.concat dir name));
  Sys.rmdir dir

let with_temp_dir f =
  match Filename.temp_dir "nodrama" "" with
  | exception Sys_error reason ->
      Error [ Diagnostic.error "cannot create temp dir: %s" reason ]
  | dir -> Fun.protect ~finally:(fun () -> remove_dir dir) (fun () -> f dir)

let command ~cc ~output dir =
  let path = Filename.concat dir in
  let runtime = List.map (fun (name, _) -> path name) Runtime_files.files in
  Filename.quote_command cc
    (flags @ [ "-I"; dir; "-o"; output; path program ] @ runtime @ [ "-lgcc" ])

let run ~cc command =
  match Sys.command command with
  | 0 -> Ok ()
  | code when code = command_not_found ->
      Error
        [
          Diagnostic.error
            ~notes:[ "a RISC-V Linux cross compiler must be on PATH" ]
            "cannot run %s" cc;
        ]
  | code -> Error [ Diagnostic.error "%s failed with exit code %d" cc code ]

let executable ~cc ~output asm =
  with_temp_dir (fun dir ->
      let* () = write_files dir asm in
      run ~cc (command ~cc ~output dir))
