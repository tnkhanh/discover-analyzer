open Dcore
module PS = Process
module EJ = Ezjsonm

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
  List.iter
    ~f:(fun (name, config) ->
      let clang_option = EJ.get_string (EJ.find config [ "clang-option" ]) in
      let discover_option =
        EJ.get_string (EJ.find config [ "discover-option" ]) in
      let full_log_dir = log_dir ^ dir ^ "/" ^ name in
      let _ = PS.run_command [ "mkdir"; "-p"; full_log_dir ] in
      let _ = print ("\n" ^ name ^ ":________") in
      let _ = print ("....." ^ EJ.value_to_string config) in
      let all_files = Array.to_list (Sys.readdir full_dir) in
      let targets =
        String.split ~on:' ' (EJ.get_string (EJ.find config [ "targets" ]))
      in
      let _ =
        List.iter targets ~f:(fun target -> print ("Target: " ^ target)) in
      (* TODO: Check if a file is a target *)
      let is_target target_list file = true in
      let files = List.filter all_files ~f:(is_target targets) in
      List.iter files ~f:(fun file ->
          let full_filepath = full_dir ^ "/" ^ file in
          let _ = print ("File: " ^ file) in
          let command =
            [ "./discover"; "--clang-option=" ^ clang_option ]
            @ String.split ~on:' ' discover_option
            @ [ full_filepath ] in
          (*let _ = List.iter command ~f:(fun str -> print ("Comm: " ^ str)) in*)
          let output = PS.run_command_get_output command in

          let output_str =
            match output with
            | Ok result -> result
            | Error msg -> msg in
          let log_file = full_log_dir ^ "/" ^ file ^ ".log" in
          Out_channel.write_all log_file ~data:output_str))
    configs_list
;;

let benchmarks = [ "PTABEN", "ptaben/ptaben-updated/basic_c_tests" ]

let main () =
  List.iter benchmarks ~f:test

let _ = main ()
