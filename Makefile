run:build
	dune exec ./bin/main.exe ./test/test_num.py

build:
	dune build

test_params:build
	dune exec ./bin/main.exe ./test/test_params.py
