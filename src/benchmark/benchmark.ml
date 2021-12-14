open Dcore
module PS = Process

let _ = print "Hello!\n"
let dir_prefix = "benchmarks/"
let log_dir = "benchmark-log/"

let test benchmark =
  let description, dir = benchmark in
  let _ = print ("Testing " ^ description ^ " in " ^ dir) in
  PS.run_command ["discover"]

let benchmarks = [
  ("PTABEN", "ptaben/ptaben-updated/basic_c_tests")]
let _ = List.iter benchmarks ~f:test
