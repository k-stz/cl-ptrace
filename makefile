ptrace-test: ptrace-test.c
build:
	gcc -o bin/ptrace-test ptrace-test.c
	gcc -o bin/return-x return-x.c
	gcc -o bin/attach-to attach-to.c

run-ptrace-test:
	./bin/ptrace-test ./bin/return-x

run-return-x:
	./bin/return-x

run-attach-to:
	./bin/attach-to
