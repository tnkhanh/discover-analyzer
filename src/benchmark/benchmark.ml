open Dcore
module PS = Process
module EJ = Ezjsonm

let _ = print "Hello!\n"
let dir_prefix = "benchmarks/"
let log_dir = "benchmark-log/"

let test benchmark =
  let description, dir = benchmark in
  let full_dir = dir_prefix ^ dir in
  let _ = print ("Testing " ^ description ^ " in " ^ dir) in
  let config_filename = full_dir ^ "/benchmark.yaml" in
  let configs_str = In_channel.read_all config_filename in
  let configs_value = Yaml.of_string_exn configs_str in
  let configs_list = EJ.get_dict configs_value in
  List.iter configs_list ~f:(fun (name, config) ->
    let clang_option = EJ.get_string (EJ.find config ["clang-option"]) in
    let discover_option = EJ.get_string (EJ.find config ["discover-option"]) in
    let targets = EJ.get_string (EJ.find config ["targets"]) in
    let _ = print ("\n" ^ name ^ ":________") in
    let _ = print (Yaml.to_string_exn config) in
    let files = 
      let ls_output = PS.run_command_get_output (["ls"; full_dir ^"/"^targets]) in
      match ls_output with
      | Ok ls_output_str -> let _ = print ls_output_str in Str.split (Str.regexp "[ \n\r\x0c\t]+") ls_output_str
      | Error msg -> let _ = print msg in [] in

    List.iter files ~f:(fun file ->
      let output = PS.run_command_get_output 
                     (["./discover"; 
                      "--clang-option=" ^ clang_option]
                      @ (String.split_on_chars ~on:[' '] discover_option)
                      @ [full_dir ^ "/" ^ file]) in

      let output_str = match output with
      | Ok result -> result
      | Error msg -> msg in
      let full_log_dir = log_dir ^ dir in
      Out_channel.write_all (full_log_dir ^ "/" ^ file) ~data:output_str
    )
  )

let benchmarks = [
  ("PTABEN", "ptaben/ptaben-updated/basic_c_tests")]
let _ = List.iter benchmarks ~f:test
