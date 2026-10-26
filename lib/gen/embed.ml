let entry path =
  let contents = In_channel.with_open_bin path In_channel.input_all in
  Printf.printf "    (%S, %S);\n" (Filename.basename path) contents

let () =
  print_endline "let files =\n  [";
  Array.to_list Sys.argv |> List.tl |> List.iter entry;
  print_endline "  ]"
