.PHONY: compile test clean

compile: rebar
	@./rebar compile

test: rebar
	@./rebar skip_deps=true eunit

clean: rebar
	@./rebar clean

rebar:
	@erl -noshell -s inets \
	     -eval 'httpc:request(get, {"http://bit.ly/getrebar", []}, [], [{stream, "rebar"}])' \
	     -s init stop
	@chmod u+x rebar
