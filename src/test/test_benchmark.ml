open Dcore
module PS = Process
module EJ = Ezjsonm

let discover_exec = ref ""
let conf_file = ref ""

type config = {
  conf_name: string;
  conf_clang_opt: string;
  conf_discover_opt: string;
  conf_targets: string list;
  conf_excludes: string list;
  conf_recurse: bool;
}

let default_config =
  {
    conf_name = "default"; 
    conf_clang_opt = "";
    conf_discover_opt= "";
    conf_targets = [".c"];
    conf_excludes = [];
    conf_recurse = false;
  }

let mk_config (name: string) (clang_opt: string) (dis_opt: string) (recurse: bool) (excludes: string list) 
      (targets: string list) =
  {
    conf_name = name;
    conf_clang_opt = clang_opt;
    conf_discover_opt = dis_opt;
    conf_recurse = recurse;
    conf_excludes = excludes;
    conf_targets = targets;
  }

let str_of_config conf =
  "Config:\nName: " ^ conf.conf_name ^
  "\nClang_opt: " ^ conf.conf_clang_opt ^
  "\nDiscover_opt: " ^ conf.conf_discover_opt ^
  "\nTargets: " ^ (String.concat ~sep:" " conf.conf_targets) ^
  "\nRecurse: " ^ (string_of_bool conf.conf_recurse)

let read_confs_from_file filename =
  let configs_str = 
    try In_channel.read_all filename 
    with e -> let _ = print ("Exception: " ^ Exn.to_string e) in "" in
  if String.equal configs_str "" then [default_config]
  else
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
      match EJ.find_opt conf_yaml ["recursive"] with
      | None -> false
      | Some opt ->
        if String.equal (EJ.get_string opt) "yes" then true else false in
    let targets =
      match EJ.find_opt conf_yaml [ "targets" ] with
      | None -> [""]
      | Some opt -> String.split ~on:' ' (EJ.get_string opt) in
    let excludes =
      match EJ.find_opt conf_yaml [ "excludes" ] with
      | None -> []
      | Some opt -> String.split ~on:' ' (EJ.get_string opt) in
    mk_config name clang_option discover_option recurse excludes targets
  )

let log_dir = "benchmark-log/"

let rec test default_conf benchmark =
  let description, dir = benchmark in
  let _ = print ("Testing " ^ description ^ " in " ^ dir) in
  let config_filename = 
    if String.equal !conf_file "" then dir ^ "/benchmark.yaml"
    else
      !conf_file
  in
  let configs = 
    if Sys.file_exists_exn config_filename then
      let _ = print ("Found config file: " ^ config_filename) in
      read_confs_from_file config_filename
    else
      let _ = print ("Config file " ^ config_filename 
                     ^ " not found. Using default config") in
      [default_conf]
  in
  List.iter
    ~f:(fun conf ->
(*      let total_assert_ok = ref 0 in*)
      (*let total_assert_failed = ref 0 in*)
      (*let total_refute_ok = ref 0 in*)
      (*let total_refute_failed = ref 0 in*)
      let total_valid_assert = ref 0 in
      let total_invalid_assert = ref 0 in

      let _ = print ("Config for " ^ dir ^ ":\n" ^ (str_of_config conf)) in
      let full_log_dir = log_dir ^ dir ^ "/" ^ conf.conf_name in
      let _ = PS.run_command [ "mkdir"; "-p"; full_log_dir ] in
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

      (* exclude needs to match the full file name *)
      let rec is_included excludes file =
        match excludes with
        | [] -> true
        | exclude :: tl ->
          if String.equal file exclude then false
          else
            is_included tl file
        in
        
      let is_test_file file = (is_target conf.conf_targets file) && (is_included conf.conf_excludes file) in

      (*let test_files = List.filter all_files ~f:is_test_file in*)
      let _ = List.iter all_files ~f:(fun file ->
          let full_filepath = dir ^ "/" ^ file in
          match Sys.is_directory full_filepath with
          | `Yes -> if conf.conf_recurse then test conf ("rec", full_filepath) else ()
          | `No ->
            if not (is_test_file file) then ()
            else
            let _ = print ("File: " ^ file) in
            let command =
              [ !discover_exec; "--clang-option=" ^ conf.conf_clang_opt ]
              @ String.split ~on:' ' conf.conf_discover_opt
              @ [ full_filepath ] in
            (*let _ = List.iter command ~f:(fun str -> print ("Comm: " ^ str)) in*)
            let output = PS.run_command_get_output command in

            let output_str =
              match output with
              | Ok result -> result
              | Error msg -> msg in

            let output_lines = String.split ~on:'\n' output_str in
            let _ = List.iter output_lines ~f:(fun line ->
              if String.is_prefix line ~prefix:__valid_assert
              then
                total_valid_assert :=
                  !total_valid_assert +
                  int_of_string (String.sub line
                                   ~pos:(String.length __valid_assert)
                                   ~len:((String.length line) - (String.length __valid_assert)))
              else
              if String.is_prefix line ~prefix:__invalid_assert
              then
                total_invalid_assert :=
                  !total_invalid_assert +
                  int_of_string (String.sub line
                                   ~pos:(String.length __invalid_assert)
                                   ~len:((String.length line) - (String.length __invalid_assert)))
              else ()

(*              if String.is_substring ~substring:__assert line then *)
                (*(if String.is_substring ~substring:_ok_status line then*)
                   (*total_assert_ok := !total_assert_ok + 1*)
                 (*else if String.is_substring ~substring:_failed_status line then*)
                   (*total_assert_failed := !total_assert_failed + 1*)
                 (*else ()*)
              (*else *)
              (*if String.is_substring ~substring:__refute line then*)
                (*(if String.is_substring ~substring:_ok_status line then*)
                   (*total_refute_ok := !total_refute_ok + 1*)
                 (*else if String.is_substring ~substring:_failed_status line then*)
                   (*total_refute_failed := !total_refute_failed + 1*)
                 (*else ()*)
              (*else*)
                (*()*)
            ) in

            let log_file = full_log_dir ^ "/" ^ file ^ ".log" in
            Out_channel.write_all log_file ~data:output_str
          | `Unknown -> ()) in
      let summary =
        "Summary of config " ^ conf.conf_name ^ " of benchmark " ^ dir ^ ":\n" ^
        "  Valid asssertions: " ^ (pr_int !total_valid_assert) ^
        "  Invalid asssertions: " ^ (pr_int !total_invalid_assert) in
      print summary)
    configs
;;

let default_benchmarks = [ "PTABEN", "benchmarks/ptaben/ptaben-updated/basic_c_tests" ]

let main () =
  let execname = Sys.argv.(0) in
  let _ = discover_exec := (Filename.dirname execname) ^ "/discover" in
  let _ = print ("Arg 0: " ^ execname) in
  let _ = print ("Discover exec: " ^ !discover_exec) in
  let usage_msg = "./test_benchmark [benchmark1] [benchmark2] [..] [-conf some_config.yaml]" in
  let benchmarks = ref [] in
  let anon_fun benchmark =
    benchmarks := ("X", benchmark) :: !benchmarks in
  let speclist =
    [("-conf", Arg.Set_string conf_file, "Set config file")] in
  let _ = Arg.parse speclist anon_fun usage_msg in
  let _ = if List.is_empty !benchmarks then benchmarks := default_benchmarks else () in
  List.iter !benchmarks ~f:(test default_config)

let _ = main ()
