open Dcore
module PS = Process
module EJ = Ezjsonm

type config = {
  conf_name: string;
  conf_clang_opt: string;
  conf_discover_opt: string;
  conf_targets: string list;
  conf_recurse: bool;
}

let mk_default_config () =
  {
    conf_name = "default"; 
    conf_clang_opt = "";
    conf_discover_opt= "";
    conf_targets = [];
    conf_recurse = false;
  }

let mk_config (name: string) (clang_opt: string) (dis_opt: string) (recurse: bool) (targets: string list) =
  {
    conf_name = name;
    conf_clang_opt = clang_opt;
    conf_discover_opt = dis_opt;
    conf_recurse = recurse;
    conf_targets = targets;
  }

let read_confs_from_file filename =
  try
    let configs_str = In_channel.read_all filename in
    let configs_value = Yaml.of_string_exn configs_str in
    let configs_list = EJ.get_dict configs_value in
    List.map configs_list ~f:(fun (name, conf_yaml) ->
      let clang_option = EJ.get_string (EJ.find conf_yaml [ "clang-option" ]) in
      let discover_option =
        EJ.get_string (EJ.find conf_yaml [ "discover-option" ]) in
      let recurse_str = 
        EJ.get_string (EJ.find conf_yaml [ "recursive" ]) in
      let recurse =
        if String.equal recurse_str "true" then true else false in
      let targets =
        String.split ~on:' ' (EJ.get_string (EJ.find conf_yaml [ "targets" ])) in

      mk_config name clang_option discover_option recurse targets
    )
  with End_of_file -> [mk_default_config()]

let dir_prefix = "benchmarks/"
let log_dir = "benchmark-log/"

let rec test benchmark =
  let description, dir = benchmark in
  let full_dir = dir_prefix ^ dir in
  let _ = print ("Testing " ^ description ^ " in " ^ dir) in
  let config_filename = full_dir ^ "/benchmarki.yaml" in
  let configs = read_confs_from_file config_filename in
  List.iter
    ~f:(fun conf ->
      let full_log_dir = log_dir ^ dir ^ "/" ^ conf.conf_name in
      let _ = PS.run_command [ "mkdir"; "-p"; full_log_dir ] in
      let _ = print ("\n" ^ conf.conf_name ^ ":________") in
      let all_files = Array.to_list (Sys.readdir full_dir) in
      let _ =
        List.iter conf.conf_targets ~f:(fun target -> print ("Target: " ^ target)) in
      (* TODO: Check if a file is a target *)
      let is_target target_list file = true in
      let files = List.filter all_files ~f:(is_target conf.conf_targets) in
      List.iter files ~f:(fun file ->
          let full_filepath = full_dir ^ "/" ^ file in
          match Sys.is_directory full_filepath with
          | `Yes -> test ("", full_filepath)
          | `No ->
            let _ = print ("File: " ^ file) in
            let command =
              [ "./discover"; "--clang-option=" ^ conf.conf_clang_opt ]
              @ String.split ~on:' ' conf.conf_discover_opt
              @ [ full_filepath ] in
            (*let _ = List.iter command ~f:(fun str -> print ("Comm: " ^ str)) in*)
            let output = PS.run_command_get_output command in

            let output_str =
              match output with
              | Ok result -> result
              | Error msg -> msg in
            let log_file = full_log_dir ^ "/" ^ file ^ ".log" in
            Out_channel.write_all log_file ~data:output_str
          | `Unknown -> ()))
    configs
;;

let benchmarks = [ "PTABEN", "ptaben/ptaben-updated/basic_c_tests" ]

let main () =
  List.iter benchmarks ~f:test

let _ = main ()
