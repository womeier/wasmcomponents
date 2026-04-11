open Stdlib

let string_to_char_list s =
  let n = String.length s in
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (String.get s i :: acc)
  in
  aux (n - 1) []

let char_list_to_string cl =
  let buf = Buffer.create 16 in
  List.iter (Buffer.add_char buf) cl;
  Buffer.contents buf

let read_file filename =
  In_channel.with_open_text filename In_channel.input_all

let read_binary_file filename =
  In_channel.with_open_bin filename (fun ch ->
    let len = in_channel_length ch in
    really_input_string ch len
  )

let print_wit_usage executable =
  Printf.eprintf "Usage: %s [--component] <file>\n" executable

let parse_wit_file filename =
  let input = read_file filename in
  let fuel = 10000 in
  match Wit_parser.parse_wit fuel (string_to_char_list input) with
  | None ->
      Printf.printf "Parse error\n";
      exit 1
  | Some pkg ->
      Printf.printf "Successfully parsed WIT package:\n";
      Printf.printf "  Namespace:  %s\n" (char_list_to_string pkg.Wit_ast.pkg_namespace);
      Printf.printf "  Name:       %s\n" (char_list_to_string pkg.Wit_ast.pkg_name);
      (match pkg.Wit_ast.pkg_version with
       | None -> ()
       | Some v -> Printf.printf "  Version:    %s\n" (char_list_to_string v));
      Printf.printf "  Interfaces: %d\n" (List.length pkg.Wit_ast.pkg_interfaces);
      Printf.printf "  Worlds:     %d\n" (List.length pkg.Wit_ast.pkg_worlds)

let parse_component_file filename =
  let input = read_binary_file filename in
  let chars = string_to_char_list input in
  match Component_binary_parser.run_parse_component_str chars with
  | Datatypes.Coq_inl _ ->
      Printf.printf "Successfully parsed component binary\n"
  | Datatypes.Coq_inr err ->
      Printf.printf "Parse error: %s\n" (char_list_to_string err);
      exit 1


let () =
  if Array.length Sys.argv < 2 || Array.length Sys.argv > 3 then (
    print_wit_usage Sys.argv.(0);
    exit 1
  );
  let parse_component,
      filename =
    if Array.length Sys.argv = 3 && Sys.argv.(1) = "--component" then
      true, Sys.argv.(2)
    else
      false, Sys.argv.(1)
  in
  if parse_component then
    parse_component_file filename
  else
    parse_wit_file filename
