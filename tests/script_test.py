import os

test_file = open("tests/basic.txt", 'r')
fouine = "./_build/default/main.exe" #path of the executable
expected_results = []

for l in test_file.readlines():

    name,code,tree,result = l.split(';')
    
    if os.fork():
        expected_results.append((name, code, tree, result[:-1]))
        
    else:
        #Writing the test code in a file
        code_file = open("tests/tmp/" + name, 'w')
        code_file.write(code + '\n')
        code_file.close()

        #Redirecting stdin and stdout
        code_file = os.open("tests/tmp/" + name, os.O_RDONLY)
        os.dup2(code_file, 0)
        os.close(code_file)

        result_file = os.open("tests/tmp/r_"+name, os.O_CREAT | os.O_WRONLY | os.O_TRUNC, 0o644)
        os.dup2(result_file, 1)
        os.close(result_file)

        #Execute the test
        os.execlp(fouine, fouine)

#Waiting for all children
for i in range(len(expected_results)): os.wait()

#Checking the results            (expected_results.append(name, code, tree, result[:-1]))
for i in range(len(expected_results)):
    results = open("tests/tmp/r_" + expected_results[i][0], 'r')
    tree = results.readline()[:-1]
    result = results.readline()[:-1]

    t_bool = tree == expected_results[i][2]
    r_bool = result == expected_results[i][3]

    print(expected_results[i][0], end=' : ')
    
    if t_bool:
        if r_bool:
            print("OK")
        else:
            print("\033[31mResults mismatched !\033[0m \n     Had :", result, "\nExpected :", expected_results[i][3], end = "\n\n")
    else:
        if r_bool:
            print("\033[31mTrees mismatched !\033[0m \n     Had :", tree, "\nExpected :", expected_results[i][2], end = "\n\n")
        else:
            print("\033[31mTrees and results mismatched !\033[0m \n     Had :", tree, "\nExpected :", expected_results[i][2],
                   "\n     Had :", result, "\nExpected :", expected_results[i][3],end = "\n\n")
