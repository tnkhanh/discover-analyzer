open Dcore
module PS = Process
module EJ = Ezjsonm

let discover_exec = ref ""
let conf_file = ref ""

type config =
  { conf_name : string;
    conf_clang_opt : string;
    conf_discover_opt : string;
    conf_targets : string list;
    conf_excludes : string list;
    conf_recurse : bool
  }

let default_config =
  { conf_name = "default";
    conf_clang_opt = "";
    conf_discover_opt = "";
    conf_targets = [ ".c" ];
    conf_excludes = [];
    conf_recurse = false
  }
;;

let mk_config
    (name : string)
    (clang_opt : string)
    (dis_opt : string)
    (recurse : bool)
    (excludes : string list)
    (targets : string list)
  =
  { conf_name = name;
    conf_clang_opt = clang_opt;
    conf_discover_opt = dis_opt;
    conf_recurse = recurse;
    conf_excludes = excludes;
    conf_targets = targets
  }
;;

let str_of_config conf =
  "Config:\nName: " ^ conf.conf_name ^ "\nClang_opt: " ^ conf.conf_clang_opt
  ^ "\nDiscover_opt: " ^ conf.conf_discover_opt ^ "\nTargets: "
  ^ String.concat ~sep:" " conf.conf_targets
  ^ "\nExcludes: "
  ^ String.concat ~sep:" " conf.conf_excludes
  ^ "\nRecurse: "
  ^ string_of_bool conf.conf_recurse
;;

let read_confs_from_file filename =
  let configs_str =
    try In_channel.read_all filename
    with e ->
      let _ = print ("Exception: " ^ Exn.to_string e) in
      "" in
  if String.equal configs_str ""
  then [ default_config ]
  else (
    let configs_value = Yaml.of_string_exn configs_str in
    let configs_list = EJ.get_dict configs_value in
    List.map configs_list ~f:(fun (name, conf_yaml) ->
        let clang_option =
          match EJ.find_opt conf_yaml [ "clang-option" ] with
          | None -> ""
          | Some opt -> EJ.get_string opt in
        let discover_option =
          match EJ.find_opt conf_yaml [ "discover-option" ] with
          | None -> ""
          | Some opt -> EJ.get_string opt in
        let recurse =
          match EJ.find_opt conf_yaml [ "recursive" ] with
          | None -> false
          | Some opt ->
            if String.equal (EJ.get_string opt) "yes" then true else false
        in
        let targets =
          match EJ.find_opt conf_yaml [ "targets" ] with
          | None -> [ "" ]
          | Some opt -> String.split ~on:' ' (EJ.get_string opt) in
        let excludes =
          match EJ.find_opt conf_yaml [ "excludes" ] with
          | None -> []
          | Some opt -> String.split ~on:' ' (EJ.get_string opt) in
        mk_config name clang_option discover_option recurse excludes targets))
;;

let log_dir = "benchmark-log/"

(* TODO: Do better than suffix check *)

let is_test_file (conf : config) (file : string) : bool =
  List.exists
    ~f:(fun target -> String.is_suffix ~suffix:target file)
    conf.conf_targets
  && List.for_all
       ~f:(fun exclude -> not (String.equal exclude file))
       conf.conf_excludes
;;

let rec test default_conf benchmark =
  let description, dir = benchmark in
  let _ = print ("Testing " ^ description ^ " in " ^ dir) in
  let config_filename =
    if String.equal !conf_file "" then dir ^ "/benchmark.yaml" else !conf_file
  in
  let configs =
    if Sys.file_exists_exn config_filename
    then (
      let _ = print ("Found config file: " ^ config_filename) in
      read_confs_from_file config_filename)
    else (
      let _ =
        print
          ("Config file " ^ config_filename
         ^ " not found. Using default config") in
      [ default_conf ]) in
  List.iter
    ~f:(fun conf ->
      (*      let total_assert_ok = ref 0 in*)
      (*let total_assert_failed = ref 0 in*)
      (*let total_refute_ok = ref 0 in*)
      (*let total_refute_failed = ref 0 in*)
      let total_valid_assert = ref 0 in
      let total_invalid_assert = ref 0 in

      let _ = print ("Config for " ^ dir ^ ":\n" ^ str_of_config conf) in
      let full_log_dir = log_dir ^ dir ^ "/" ^ conf.conf_name in
      let _ = PS.run_command [ "mkdir"; "-p"; full_log_dir ] in
      let all_files =
        List.sort ~compare:Poly.compare (Array.to_list (Sys.readdir dir)) in

      let update_summary test_output =
        let output_lines = String.split ~on:'\n' test_output in
        List.iter
          ~f:(fun line ->
            if String.is_prefix ~prefix:__report_valid_assert line
            then (
              let prefix_length = String.length __report_valid_assert in
              let number_length =
                String.length line - String.length __report_valid_assert in
              let added_valid_assert =
                int_of_string
                  (String.sub line ~pos:prefix_length ~len:number_length) in
              total_valid_assert := !total_valid_assert + added_valid_assert)
            else if String.is_prefix ~prefix:__report_invalid_assert line
            then (
              let prefix_length = String.length __report_invalid_assert in
              let number_length =
                String.length line - String.length __report_invalid_assert
              in
              let added_invalid_assert =
                int_of_string
                  (String.sub line ~pos:prefix_length ~len:number_length) in

              total_invalid_assert
                := !total_invalid_assert + added_invalid_assert))
          output_lines in

      let _ =
        List.iter
          ~f:(fun file ->
            let full_filepath = dir ^ "/" ^ file in
            match Sys.is_directory full_filepath with
            | `Yes -> if conf.conf_recurse then test conf ("rec", full_filepath)
            | `No ->
              if is_test_file conf file
              then (
                let _ = print ("File: " ^ file) in
                let command =
                  [ !discover_exec; "--clang-option=" ^ conf.conf_clang_opt ]
                  @ String.split ~on:' ' conf.conf_discover_opt
                  @ [ full_filepath ] in
                (*let _ = List.iter command ~f:(fun str -> print ("Comm: " ^ str)) in*)
                let log_file = full_log_dir ^ "/" ^ file ^ ".log" in
                let _ = PS.run_command_output_to_file command log_file in
                let output_str = In_channel.read_all log_file in
                update_summary output_str)
            | `Unknown -> ())
          all_files in
      let summary =
        "Summary of config " ^ conf.conf_name ^ " of benchmark " ^ dir ^ ":\n"
        ^ "  Valid asssertions: " ^ pr_int !total_valid_assert
        ^ "  Invalid asssertions: "
        ^ pr_int !total_invalid_assert in
      print summary)
    configs
;;

let default_benchmarks =
  [ "PTABEN", "benchmarks/ptaben/ptaben-updated/basic_c_tests" ]
;;

let main () =
  let execname = Sys.argv.(0) in
  let _ = discover_exec := Filename.dirname execname ^ "/discover" in
  let _ = print ("Exec file: " ^ execname) in
  let _ = print ("Discover exec: " ^ !discover_exec) in
  let usage_msg =
    "./test_benchmark [benchmark1] [benchmark2] [..] [-conf some_config.yaml]\nif no benchmark is specified, default benchmark PTABEN is run"
  in
  let benchmarks = ref [] in
  let anon_fun benchmark = benchmarks := ("X", benchmark) :: !benchmarks in
  let speclist = [ "-conf", Arg.Set_string conf_file, "Set config file" ] in
  let _ = Arg.parse speclist anon_fun usage_msg in
  let _ =
    if List.is_empty !benchmarks then benchmarks := default_benchmarks else ()
  in
  List.iter ~f:(test default_config) !benchmarks
;;

let _ = main ()
