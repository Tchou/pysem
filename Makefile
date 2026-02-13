run:build
	dune exec ./bin/main.exe ./test/test_num.py

build:
	dune build

clean:
	dune clean

test_params:build
	dune exec ./bin/main.exe ./test/test_params.py

pystate:build
	dune exec ./bin/main.exe ./test/test_state.py
