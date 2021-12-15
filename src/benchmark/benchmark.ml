open Dcore
module PS = Process

let _ = print "Hello!\n"
let dir_prefix = "benchmarks/"
let log_dir = "benchmark-log/"

let test benchmark =
  let description, dir = benchmark in
  let full_dir = dir_prefix ^ dir in
  let _ = print ("Testing " ^ description ^ " in " ^ dir) in
  let config_filename = full_dir ^ "/benchmark.yaml" in
  let config_str = In_channel.read_all config_filename in
  let config_value = Yaml.of_string_exn config_str in
  print (Yaml.to_string_exn config_value)
(*  let output = PS.run_command_get_output ["./discover"; "--clang-option=-I lib/discover";
                  "--dfa-pointer"; 
                  "benchmarks/ptaben/ptaben-updated/basic_c_tests/global-array.c"; "-d"] in


  match output with
  | Ok result -> print ("Output: " )
  | Error msg -> print ("Error messsage: ") *)

let benchmarks = [
  ("PTABEN", "ptaben/ptaben-updated/basic_c_tests")]
let _ = List.iter benchmarks ~f:test
