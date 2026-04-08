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

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s <file.wit>\n" Sys.argv.(0);
    exit 1
  );
  let filename = Sys.argv.(1) in
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
