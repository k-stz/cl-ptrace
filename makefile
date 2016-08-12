ptrace-test: ptrace-test.c
build:
	gcc -o bin/ptrace-test ptrace-test.c -I.
run:
	./bin/ptrace-test

