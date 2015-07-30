all:
	erl -make

.PHONY: test clean

test:
	erl -noshell -pa ebin -eval 'eunit:test("ebin", [verbose])' -s init stop

clean:
	rm -rf ebin/*
