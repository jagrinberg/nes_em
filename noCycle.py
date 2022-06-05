file1 = open("nestest.txt", 'r')

lines = [(x[:73]+"\n") for x in file1.readlines()]

file2 = open("cputest.txt", 'w')

file2.writelines(lines)