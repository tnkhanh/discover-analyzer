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

type total_result =
  { tot_correct : int;
    tot_incorrect : int;
    tot_missing : int;
    tot_error_files : string list
  }

let empty_result =
  { tot_correct = 0; tot_incorrect = 0; tot_missing = 0; tot_error_files = [] }
;;

let sum_result (res1 : total_result) (res2 : total_result) : total_result =
  { tot_correct = res1.tot_correct + res2.tot_correct;
    tot_incorrect = res1.tot_incorrect + res2.tot_incorrect;
    tot_missing = res1.tot_missing + res2.tot_missing;
    tot_error_files = res1.tot_error_files @ res2.tot_error_files
  }
;;

let find_stat_int (lines : string list) (prefix : string) : int =
  List.fold ~init:0
    ~f:(fun acc line ->
      if String.is_prefix ~prefix line
      then (
        let strs = String.split ~on:':' line in
        int_of_string (String.lstrip (List.nth_exn strs 1)))
      else acc)
    lines
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

let collect_result (lines : string list) : total_result =
  { tot_correct = find_stat_int lines __report_correct_bug;
    tot_incorrect = find_stat_int lines __report_incorrect_bug;
    tot_missing = find_stat_int lines __report_missing_bug;
    tot_error_files = []
  }
;;

let rec run_bench (default_conf : config) (dir : string) : total_result =
  let _ = print ("Testing " ^ dir) in
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
  List.fold ~init:empty_result
    ~f:(fun acc_result conf ->
      let total_assert_ok = ref 0 in
      let total_assert_failed = ref 0 in
      let total_refute_ok = ref 0 in
      let total_refute_failed = ref 0 in

      let _ = print ("Config for " ^ dir ^ ":\n" ^ str_of_config conf) in
      let full_log_dir = log_dir ^ dir ^ "/" ^ conf.conf_name in
      let _ = PS.run_command [ "mkdir"; "-p"; full_log_dir ] in
      let all_files =
        List.sort ~compare:Poly.compare (Array.to_list (Sys.readdir dir)) in

      let collect_summary output_lines =
        let rev_assertions =
          List.fold ~init:[]
            ~f:(fun acc line ->
              if String.is_substring ~substring:"__assert_" line
              then
                if String.is_suffix ~suffix:"OK!" line
                then (
                  let _ = total_assert_ok := !total_assert_ok + 1 in
                  line :: acc)
                else if String.is_suffix ~suffix:"FAILED!" line
                then (
                  let _ = total_assert_failed := !total_assert_failed + 1 in
                  line :: acc)
                else acc
              else if String.is_substring ~substring:"__refute_" line
              then
                if String.is_suffix ~suffix:"OK!" line
                then (
                  let _ = total_refute_ok := !total_refute_ok + 1 in
                  line :: acc)
                else if String.is_suffix ~suffix:"FAILED!" line
                then (
                  let _ = total_refute_failed := !total_refute_failed + 1 in
                  line :: acc)
                else acc
              else acc)
            output_lines in
        let assertions = List.rev rev_assertions in
        let _ =
          print ("  Found " ^ pr_int (List.length assertions) ^ " assertions:")
        in
        List.iter ~f:print assertions in

      let conf_res =
        List.fold ~init:empty_result
          ~f:(fun acc_res file ->
            let full_filepath = dir ^ "/" ^ file in
            let file_res =
              match Sys.is_directory full_filepath with
              | `Yes ->
                if conf.conf_recurse
                then run_bench conf full_filepath
                else empty_result
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
                  let output_lines = String.split ~on:'\n' output_str in
                  let _ = collect_summary output_lines in
                  let raw_res = collect_result output_lines in
                  if raw_res.tot_incorrect = 0 && raw_res.tot_missing = 0
                  then raw_res
                  else { raw_res with tot_error_files = [ full_filepath ] })
                else empty_result
              | `Unknown -> empty_result in
            sum_result acc_res file_res)
          all_files in
      let summary =
        "Summary of config " ^ conf.conf_name ^ " of benchmark " ^ dir ^ ":\n"
        ^ "  Valid asssert: " ^ pr_int !total_assert_ok ^ "\n"
        ^ "  Invalid asssert: "
        ^ pr_int !total_assert_failed
        ^ "\n" ^ "  Valid refute: " ^ pr_int !total_refute_ok ^ "\n"
        ^ "  Invalid refute: "
        ^ pr_int !total_refute_failed
        ^ "\n" in
      let _ = print summary in
      sum_result acc_result conf_res)
    configs
;;

let default_benchmarks = [ "benchmarks/ptaben/ptaben-updated/basic_c_tests" ]

let main () =
  let execname = Sys.argv.(0) in
  let _ = discover_exec := Filename.dirname execname ^ "/discover" in
  let _ = print ("Exec file: " ^ execname) in
  let _ = print ("Discover exec: " ^ !discover_exec) in
  let usage_msg =
    "./test_benchmark [benchmark1] [benchmark2] [..] [-conf some_config.yaml]\nif no benchmark is specified, default benchmark PTABEN is run"
  in
  let benchmarks = ref [] in
  let anon_fun benchmark = benchmarks := benchmark :: !benchmarks in
  let speclist = [ "-conf", Arg.Set_string conf_file, "Set config file" ] in
  let _ = Arg.parse speclist anon_fun usage_msg in
  let _ =
    if List.is_empty !benchmarks then benchmarks := default_benchmarks else ()
  in
  let total_res =
    List.fold ~init:empty_result
      ~f:(fun acc dir ->
        let res = run_bench default_config dir in
        sum_result acc res)
      !benchmarks in
  let summary =
    "Total correct bug reports: "
    ^ pr_int total_res.tot_correct
    ^ "\n" ^ "Total incorrect bug reports: "
    ^ pr_int total_res.tot_incorrect
    ^ "\n" ^ "Total missing bugs: "
    ^ pr_int total_res.tot_missing
    ^ "\n" ^ "Files with errors: "
    ^ pr_list_plain ~f:pr_str total_res.tot_error_files in
  print summary
;;

let _ = main ()
