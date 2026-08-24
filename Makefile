RUN=dune exec --
MAINEXE=./bin/main.exe
DEBUG=-debug
SUMUP=-sumup

RUNM=$(RUN) $(MAINEXE)

run:build
	$(RUNM) ./test/test_num.py

build:
	dune build

clean:
	dune clean


test_params:build
	$(RUNM) ./test/test_params.py

test_paramsd:build
	$(RUNM) $(DEBUG) ./test/test_params.py


pystate:build
	$(RUNM) ./test/test_state.py

pystated:build
	$(RUNM) $(DEBUG) ./test/test_state.py

pstest:build
	$(RUNM) $(SUMUP) ./test/pystate/*
