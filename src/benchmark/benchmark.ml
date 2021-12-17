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
    let full_log_dir = log_dir ^ dir ^ "/" ^ name in
    let _ = PS.run_command ["mkdir"; "-p"; full_log_dir] in

    (*let targets = EJ.get_string (EJ.find config ["targets"]) in*)
    let _ = print ("\n" ^ name ^ ":________") in
    let _ = print ("....." ^ (EJ.value_to_string config)) in
    let files = 
(*      let ls_output = PS.run_command_get_output ["ls"; full_dir^"/*.c" [>^"/"^targets <]] in*)
      (*match ls_output with*)
      (*| Ok ls_output_str -> let _ = print ls_output_str in Str.split (Str.regexp "[ \n\r\x0c\t]+") ls_output_str*)
      (*| Error msg -> let _ = print msg in [] in*)
      (*FileUtil.ls full_dir in*)
      Array.to_list (Sys.readdir full_dir) in

    List.iter files ~f:(fun file ->
      let full_filepath = full_dir ^ "/" ^ file in
      let _ = print ("File: " ^ full_filepath) in
      let command = ["./discover"; 
                      "--clang-option=" ^ clang_option]
                      @ (String.split_on_chars ~on:[' '] discover_option)
                      @ [full_filepath] in
      let _ = List.iter command ~f:(fun str -> print ("Comm: " ^ str)) in
      let output = PS.run_command_get_output command in

      let output_str = match output with
      | Ok result -> result
      | Error msg -> msg in
      let log_file = full_log_dir ^ "/" ^ file^".log" in
      let _ = print ("Log: " ^ log_file) in
      Out_channel.write_all log_file ~data:output_str
    )
  )
;;

let _ =
  let ls_test = PS.run_command_get_output ["ls"; "*.c"] in
  let ls_str = match ls_test with
    | Ok str -> str
    | Error str -> str in
  print ("ls:...\n" ^ ls_str)
;;

let benchmarks = [
  ("PTABEN", "ptaben/ptaben-updated/basic_c_tests")]
let _ = List.iter benchmarks ~f:test
