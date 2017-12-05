.PHONY: all compile xref dialyzer test

all: xref

compile:
	@rebar3 compile

xref:
	@rebar3 xref

dialyzer:
	@rebar3 dialyzer

test: xref
	@rebar3 ct

clean:
	@rebar3 clean

distclean: clean
	@rm -rfv _build
