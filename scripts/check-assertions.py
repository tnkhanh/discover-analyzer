#!/usr/bin/env python3

# Example:
#   ./check-assertions.py --clang-libs "../lib/discover.h"
#                         --options "--dfa-alias --dfa-inter"
#                         /path/to/a/benchmark/folder
#

import argparse
import os
import sys
import signal
import subprocess
import glob
import time
import re
import json

from pathlib import Path
from threading import Timer

# strings to parse Discover's output
ASSERT = "__assert_"
REFUTE = "__refute_"
ERROR = "ERROR"
TIMEOUT = "TIMEOUT!!"
OK = "OK!"
FAILED = "FAILED!"
CHECKED_ASSERTS = "assertion(s) are checked"
NO_ASSERT = "No assertion is found!"
FOUND_ASSERTS = "Found total assertions"

# environment variables
current_dir = os.getcwd()
project_dir = ""
discover_exe = ""
benchmark_dir = ""
recursive_benchmark = False
analyzer_timeout = 10

# summary variables
total_assert_ok = 0
total_assert_failed = 0
total_refute_ok = 0
total_refute_failed = 0
total_file_timeout = 0
total_file_error = 0
total_commands = 0
total_checked_command = 0


# parse arguments
def parse_args():
    parser = argparse.ArgumentParser(description='Run benchmark')
    parser.add_argument('benchmark_dir',
                        help='Path to the benchmark')
    parser.add_argument("--recursive",
                        dest='recursive', action='store_true',
                        help="Support recursive benchmarks")
    parser.add_argument('--discover-option',
                        dest='discover_option', default='',
                        help='Options for Discover')
    parser.add_argument('--clang-option',
                        dest='clang_option', default='',
                        help='Options passed to Clang')
    parser.add_argument('--timeout',
                        dest='timeout', type=int, default=0,
                        help='Timeout for each test case')
    args = parser.parse_args()
    return args

def atoi(text):
    return int(text) if text.isdigit() else text

def natural_keys(text):
    return [ atoi(c) for c in re.split(r'(\d+)', text) ]

# find project dir path
def find_project_path():
    def find_path(path):
        current_path = Path(path)
        discover_exe = Path(path + "/discover")
        if discover_exe.exists():
            return path
        else:
            return find_path(str(current_path.parent))

    return find_path(current_dir)


# find test cases
def find_test_cases(benchmark_dir):
    files = []

    if recursive_benchmark:
        for fname in glob.iglob(benchmark_dir + '**/**.c', recursive=True):
            files.append(fname)
        for fname in glob.iglob(benchmark_dir + '**/**.cpp', recursive=True):
            files.append(fname)
    else:
        for fname in glob.iglob(benchmark_dir + '/*.c', recursive=False):
            files.append(fname)
        for fname in glob.iglob(benchmark_dir + '/*.cpp', recursive=False):
            files.append(fname)

    files.sort(key=natural_keys)
    return files


def kill_process(process):
    try:
        process.kill()
        os.killpg(os.getpgid(process.pid), signal.SIGTERM)   # kill sub-processes
    except Exception:
        pass
    finally:
        pass

def print_test_error(output):
    global total_commands
    for line in output.splitlines():
        if FOUND_ASSERTS in line:
            [a] = re.findall(r"\d+", line)
            total_commands += int(a)
            num_skipped = str(a) + "/" + str(a)
            print("  " + line.strip())
            print("  " + num_skipped + " assertion(s) are skipped!")

def log_error(testcase, error):
    path = Path(testcase)
    logfilename = str(path.parent) + "/logs/" + str(path.name) + ".error.log"
    logfile = open(logfilename, "w")
    logfile.write(error)
    logfile.close()
    print("  > AN ERROR OCCURRED!!!\n" +
          "  > LOG FILE:\n" +
          "  > " + logfilename)

def log_output(testcase, output):
    path = Path(testcase)
    logfilename = str(path.parent) + "/logs/" + str(path.name) + ".output.log"
    logfile = open(logfilename, "w")
    logfile.write(output)
    logfile.close()


# running Discover against all test cases
def run_discover(discover_exe, benchmark_dir, options):
    global total_assert_ok, total_assert_failed
    global total_refute_ok, total_refute_failed
    global total_commands, total_checked_command
    global total_file_timeout, total_file_error
    global analyzer_timeout

    for testcase in find_test_cases(benchmark_dir):
        # obtain specific configuration for each test case
        testcase_cfg = testcase + ".cfg.json"
        testcase_cfg_path = Path(testcase_cfg)
        if testcase_cfg_path.exists():
            f = open(testcase_cfg)
            configure = json.load(f)
            if "clang-extra-options" in configure:
                clang_extra_options = configure["clang-extra-options"]
                options = options + ["--clang-extra-option", clang_extra_options]

        discover_command = [discover_exe, testcase] + options
        print("\n" + testcase + ": ", end='')

        # run discover
        runtime = ""
        process = subprocess.Popen(discover_command,
                                   stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE,
                                   preexec_fn=os.setsid)
        killtimer = Timer(analyzer_timeout, kill_process, [process])

        try:
            time_begin = time.time()
            killtimer.start()
            stdout, stderr = process.communicate()
            time_end = time.time()
            runtime = str(float("{0:.2f}".format(time_end - time_begin))) + "s"
        except Exception:
            runtime = "Exception"
        finally:
            if killtimer.is_alive():
                killtimer.cancel()
            else:
                total_file_timeout += 1
                runtime = TIMEOUT

        print(runtime)

        # read and process output and error
        output = stdout.decode("utf-8")
        if stderr:
            total_file_error += 1
            log_error(testcase, stderr.decode("utf-8"))
            print_test_error(output)
        elif ERROR in output:
            total_file_error += 1
            log_error(testcase, output)
            print_test_error(output)
        else:
            # print("Output:\n" + stdout.decode("utf-8"))
            log_output(testcase, output)
            num_asserts = 0
            for line in output.splitlines():
                if ASSERT in line:
                    if OK in line:
                        print("  " + line.strip())
                        total_assert_ok += 1
                    elif FAILED in line:
                        print("  " + line.strip())
                        total_assert_failed += 1
                elif REFUTE in line:
                    if OK in line:
                        print("  " + line.strip())
                        total_refute_ok += 1
                    elif FAILED in line:
                        print("  " + line.strip())
                        total_refute_failed += 1
                elif CHECKED_ASSERTS in line:
                    match = re.findall(r"\d+",line)
                    num_checked = int(match[0])
                    total_checked_command += num_checked
                    print("  " + line.strip())
                elif FOUND_ASSERTS in line:
                    match = re.findall(r"\d+", line)
                    num_asserts = int(match[0])
                    total_commands += num_asserts
                    print("  " + line.strip())
                elif (NO_ASSERT in line) or (CHECKED_ASSERTS in line):
                    print("  " + line.strip())

            if (TIMEOUT in runtime):
                num_total, num_skipped = str(num_asserts), str(num_asserts)
                print("  0/" + num_total + " assertion(s) are checked, " +
                      num_skipped + " are skipped!")



def exit_gracefully(signal, frame):
    print("")
    sys.exit(0)


# main function
def main():
    global project_dir, benchmark_dir, recursive_benchmark, analyzer_timeout

    # parse argument
    arg = parse_args()
    benchmark_dir = arg.benchmark_dir
    options = arg.discover_option.split()
    if arg.clang_option:
        options = options + ["--clang-option", arg.clang_option]
    if arg.recursive:
        recursive_benchmark = True
    if arg.timeout > 0:
        analyzer_timeout = arg.timeout

    project_dir = find_project_path()
    discover_exe = os.path.relpath(project_dir + "/discover", current_dir)

    setting = ("\n******************************\n" +
               "TESTING THE ANALYZER DISCOVER..." + "\n\n" +
               "Settings:\n" +
               "  Analyzer: " + discover_exe + "\n" +
               "  Options: " + ' '.join(options) + "\n" +
               "  Benchmark: " + benchmark_dir + "\n" +
               "  Recursive: " + str(recursive_benchmark) + "\n")
    print(setting)

    print("Start testing...")

    time_begin = time.time()
    run_discover(discover_exe, benchmark_dir, options)
    time_end = time.time()
    total_time = float("{0:.2f}".format(time_end - time_begin))

    summary = ("\n**************************\n" +
               "Summary of checking assertions:\n" +
               "  Assert valid: " + str(total_assert_ok) +
               ", failed: " + str(total_assert_failed) + "\n" +
               "  Refute valid: " + str(total_refute_ok) +
               ", failed: " + str(total_refute_failed) + "\n" +
               "  Total commands: " + str(total_commands) +
               ", skipped: " + str(total_commands - total_checked_command) + "\n" +
               "  Timeout files: " + str(total_file_timeout) + "\n" +
               "  Error files: " + str(total_file_error) + "\n\n" +
               "Total run time: " + str(total_time) + "s\n")
    print(summary)


if __name__ == "__main__":
    signal.signal(signal.SIGINT, exit_gracefully)
    main()
