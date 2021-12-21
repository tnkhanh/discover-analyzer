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

let default_config =
  {
    conf_name = "default"; 
    conf_clang_opt = "";
    conf_discover_opt= "";
    conf_targets = [".c"];
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

let str_of_config conf =
  "Config:\nName: " ^ conf.conf_name ^
  "\nClang_opt: " ^ conf.conf_clang_opt ^
  "Discover_opt: " ^ conf.conf_discover_opt ^
  "\nTargets: " ^ (String.concat ~sep:" " conf.conf_targets) ^
  "Recurse: " ^ (string_of_bool conf.conf_recurse)

let read_confs_from_file filename =
  let _ = print filename in
  let configs_str = 
    try In_channel.read_all filename 
    with e -> let _ = print ("Exn: " ^ Exn.to_string e) in "" in
  if String.equal configs_str "" then [default_config]
  else
  let _ = print configs_str in
  let configs_value = Yaml.of_string_exn configs_str in
  let configs_list = EJ.get_dict configs_value in
  List.map configs_list ~f:(fun (name, conf_yaml) ->
    let clang_option = EJ.get_string (EJ.find conf_yaml [ "clang-option" ]) in
    let discover_option =
      EJ.get_string (EJ.find conf_yaml [ "discover-option" ]) in
    let recurse =
      match EJ.find_opt conf_yaml ["recursive"] with
      | None -> false
      | Some value ->
        if String.equal (EJ.get_string value) "true" then true else false in
    let targets =
      String.split ~on:' ' (EJ.get_string (EJ.find conf_yaml [ "targets" ])) in

    mk_config name clang_option discover_option recurse targets
  )

let log_dir = "benchmark-log/"

let rec test ?(conf_file="") benchmark =
  let description, dir = benchmark in
  let _ = print ("Testing " ^ description ^ " in " ^ dir) in
  let config_filename = 
    if String.equal conf_file "" then dir ^ "/benchmark.yaml"
    else
      conf_file
  in
  let configs = read_confs_from_file config_filename in
  List.iter
    ~f:(fun conf ->
      let full_log_dir = log_dir ^ dir ^ "/" ^ conf.conf_name in
      let _ = PS.run_command [ "mkdir"; "-p"; full_log_dir ] in
      let _ = print (str_of_config conf) in
      let _ = List.iter conf.conf_targets ~f:print in
      let all_files = Array.to_list (Sys.readdir dir) in
      let _ =
        List.iter conf.conf_targets ~f:(fun target -> print ("Target: " ^ target)) in
      (* TODO: Do better than suffix check *)
      let rec is_target target_list file = 
        match target_list with
        | [] -> false
        | target :: tl ->
          if String.is_suffix file ~suffix:target then true
          else
            is_target tl file
        in
      (* TODO: Make a function that runs each file *)
      let files = List.filter all_files ~f:(is_target conf.conf_targets) in
      List.iter files ~f:(fun file ->
          let full_filepath = dir ^ "/" ^ file in
          match Sys.is_directory full_filepath with
          | `Yes -> test ~conf_file:config_filename ("", full_filepath)
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

let default_benchmarks = [ "PTABEN", "benchmarks/ptaben/ptaben-updated/basic_c_tests" ]

let main () =
  let usage_msg = "./test_benchmark [benchmark1] [benchmark2]" in
  let benchmarks = ref [] in
  let arg_conf_file = ref "" in
  let anon_fun benchmark =
    benchmarks := ("X", benchmark) :: !benchmarks in
  let speclist =
    [("-conf", Arg.Set_string arg_conf_file, "Set config file")] in
  let _ = Arg.parse speclist anon_fun usage_msg in
  let _ = if List.is_empty !benchmarks then benchmarks := default_benchmarks else () in
  List.iter !benchmarks ~f:test

let _ = main ()
