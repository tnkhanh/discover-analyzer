# -*- coding: UTF-8 -*-

import sys
import os
import subprocess
import csv
import glob
import re
import time

from threading import Timer
#environment variables
recursive_benchmark = True

TIMEOUT = "TIMEOUT!!"

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
    return [atoi(c) for c in re.split(r'(\d+)', text)]
# find test cases
def find_test_cases(benchmark_dir):
    files = []

    if recursive_benchmark:
        for fname in glob.iglob(benchmark_dir + '**/**.bc', recursive=True):
            if "log" not in fname :
                files.append(fname)
    else:
        for fname in glob.iglob(benchmark_dir + '/*.bc', recursive=False):
             if "log" not in fname:
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

def run_svf_test(discover_path, discover_option, benchmark_dir,output_path,analyzer_timeout):
    # 
    total_res = output_path + "/" + "total_res.txt"
    with open(total_res, 'w') as total_f:
        total_res = ""
        # print(find_test_cases(benchmark_dir))
        for testcase in find_test_cases(benchmark_dir):
            command = [discover_path]+discover_option.split()+[testcase]
            print(command)
            test_res = output_path + "/" + testcase.split('/')[-1]+".txt"
            # run command
            runtime = ""
            process = subprocess.Popen(command,
                                    stdout=subprocess.PIPE,
                                    stderr=subprocess.PIPE
                                    #preexec_fn=os.setsid
                                    )
            # killtimer = Timer(analyzer_timeout, kill_process, [process])
            # try:
            time_begin = time.time()
            print("time_begin " + str(float("{0:.2f}".format(time_begin))))
            #killtimer.start()
            stdout, stderr = process.communicate()
            time_end = time.time()
            print("time_end " + str(float("{0:.2f}".format(time_end))))
            runtime = str(float("{0:.2f}".format(time_end - time_begin))) + "s"
            # except Exception:
            #     runtime = "Exception"
            # finally:
            #     if killtimer.is_alive():
            #         killtimer.cancel()
            #         total_res += testcase + " " + runtime +'/n'
            #         print(testcase+" finished " + runtime)
            #     else:
            #         runtime = TIMEOUT
            #         total_res += testcase + " " + runtime +'/n'
            #         print(testcase+" "+runtime)

            res_test = stdout.decode('utf-8')
            print ("Output: " + res_test)
            res_test = res_test[res_test.rfind("Summary")-1:]
            if TIMEOUT in runtime:
                res_test += '/n'+TIMEOUT
            with open(test_res, "w") as test_f:
                test_f.write(res_test)
            test_f.close()

        total_f.write(total_res)
    total_f.close()





def main():
    # print(sys.argv)
    discover_path = sys.argv[1]
    discover_option = sys.argv[2]
    benchmark_dir = sys.argv[3]
    output_path = sys.argv[4]
    analyzer_timeout = int(sys.argv[5])
    run_svf_test(discover_path,discover_option,benchmark_dir,output_path,analyzer_timeout)


if __name__ == "__main__":
    main()

#useage as 
#python3 run_big_test.py ./discover "--dfa-pointer --dfa-inter --dfa-path-insensitive --no-debug --no-print"  ../discover-benchmarks/newest_version/output/new/coreutils-8.31/ ./output_new 600
