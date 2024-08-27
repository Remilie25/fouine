import os
from time import localtime

##Functions

def get_result(file):
    '''This function concatenates all remaining lines replacing '\n' by "EOL".'''
    result = ""

    for line in file.readlines():
        if line != '\n':
            result += line[:-1] + " EOL "

    return result

##Main program

test_file = open("tests/basic.txt", 'r')
fouine = "./_build/default/main.exe" #path of the executable
expected_results = []

for l in test_file.readlines():

    name,code,tree,result = l.split("/|/")
    
    if os.fork():
        expected_results.append((name, code, tree, result[:-1]))
        
    else:
        #Writes the test code in a file
        code_file = open("tests/tmp/" + name, 'w')
        code_file.write(code.replace(" EOL ",'\n') + ';;')
        code_file.close()

        #Redirects stdin and stdout
        code_file = os.open("tests/tmp/" + name, os.O_RDONLY)
        os.dup2(code_file, 0)
        os.close(code_file)

        result_file = os.open("tests/tmp/r_"+name, os.O_CREAT | os.O_WRONLY | os.O_TRUNC, 0o644)
        os.dup2(result_file, 1)
        os.close(result_file)
        
        #Execute the test
        os.execlp(fouine, fouine)

test_file.close()

#Waits for all children
for i in range(len(expected_results)): os.wait()

results_matched = []

#Store only results that did not matched (() for the others).
results_given = []

#Checks the results            (expected_results.append(name, code, tree, result[:-1]))
for i in range(len(expected_results)):
    results = open("tests/tmp/r_" + expected_results[i][0], 'r')
    tree = results.readline()[:-1]
    result = get_result(results)

    t_bool = tree == expected_results[i][2]
    r_bool = result == expected_results[i][3]
    results_matched.append(t_bool and r_bool)

    if t_bool and r_bool:
        results_given.append(())
    else:
        results_given.append((tree, result))

    print(expected_results[i][0], end=' : ')

    result = result.replace(" EOL ","\n")
    
    if t_bool:
        if r_bool:
            print("OK")
        else:
            print("\033[31mResults mismatched !\033[0m \n     Had :", result, "Expected :", expected_results[i][3].replace(" EOL ","\n"))
    else:
        if r_bool:
            print("\033[31mTrees mismatched !\033[0m \n     Had :", tree, "\nExpected :", expected_results[i][2], end = "\n\n")
        else:
            print("\033[31mTrees and results mismatched !\033[0m \n     Had :", tree, "\nExpected :", expected_results[i][2],
                   "\n     Had :", result, "Expected :", expected_results[i][3].replace(" EOL ","\n"))

#asks : rewrite the correct result ?
question = "Voulez-vous redefinir le resultat attendu pour %s? (type '?' for help) : "
answer = ''
replace_line = [False for _ in range(len(expected_results))]

for i in range(len(expected_results)):
    if not(results_matched[i]):
        if not(answer in ['!', 'q']):

            answer = ''
            while not(answer in ['y', 'n', '!', 'q']):
                answer = input(question%(expected_results[i][0]))
                if answer == '?':
                    print("Type :\n'y' to replace the expected result.\n'n' to skip this result.\n'!' to replace all mismatches.\n'q' to quit.\n'?' to display this help.")
            
            if answer in ['y', '!']:
                replace_line[i] = True
               
        elif answer == '!':
            replace_line[i] = True
        else:
            break

#Replaces if needed
if True in replace_line:
    test_file = open("tests/basic.txt", 'r')
    new_tf = open("tests/new_basic.txt", 'w')
    i = 0
    c = 0
    
    for l in test_file.readlines():
        if replace_line[i] and results_given[i] != ():
            new_line = "%s/|/%s/|/%s/|/%s\n"%(expected_results[i][0],
                        expected_results[i][1], results_given[i][0], results_given[i][1])
            new_tf.write(new_line)
            c += 1
               
        else:
            new_tf.write(l)

        i += 1

    test_file.close()
    new_tf.close()

    plurial = ''
    if c > 1:
        plurial = 's'
    print("Replaced %d result%s"%(c,plurial))
    
    os.execlp("mv", "mv", "tests/new_basic.txt", "tests/basic.txt")
