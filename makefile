ptrace-test: ptrace-test.c
build:
	gcc -o bin/ptrace-test ptrace-test.c
	gcc -o bin/return-x return-x.c

run:
	./bin/ptrace-test ./bin/return-x

