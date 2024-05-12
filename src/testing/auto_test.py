
import os
import sys

test_strings = ["ryzzbuzz", "recursion", "operators", "scoping"]
GEN_MODE = False

def parse_test_paths(test_string, test_type):
    ref_string = f'testing/ref/{test_string}_{test_type}.ref'
    out_string = f'testing/tmp/{test_string}_{test_type}.out'
    return ref_string, out_string
   

def gen_commands(test_string, gen_truth = False):
    test_path = f"testing/{test_string}.ryzz"
    sem_ref, sem_out   = parse_test_paths(test_string, 'semantics')
    parse_ref, parse_out = parse_test_paths(test_string, 'parsing')
    ryzz_ref, ryzz_out = parse_test_paths(test_string, 'ryzz')
    
    parse_cmd = f"./parse_test.native < {test_path} > {parse_ref if gen_truth else parse_out}"
    semant_cmd = f"./semantic_test.native < {test_path} > {sem_ref if gen_truth else sem_out}"
    ryzz_cmd = f"./ryzz.native < {test_path} > {ryzz_ref if gen_truth else ryzz_out}"
    return [('parsing', parse_cmd), ('semantics', semant_cmd), ('ryzz', ryzz_cmd)]

def ensure_same(new_path, ref_path):
    with open(new_path, 'r') as f:
        new_cont = f.read()

    with open(ref_path, 'r') as f:
        ref_cont = f.read()
    return new_cont == ref_cont

for test_string in test_strings:
    for test_type, cmd in gen_commands(test_string, GEN_MODE):
        try:
            print(f"running cmd: {cmd}")
            os.system(cmd)
        except:
            print(f"Failed test {test_type} {test_string}")
            continue
        if not GEN_MODE:
            ref, out = parse_test_paths(test_string, test_type)
            if not ensure_same(ref, out):
                print(f"Failed test {test_type} {test_string}")
            else:
                print(f"Passed test {test_type} {test_string}")
        else:
            print(f"Generated test {test_type} {test_string}")