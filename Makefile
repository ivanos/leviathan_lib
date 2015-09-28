.PHONY: test compile run deps qc qc_compile

cookie ?= dobby

all: rebar compile

compile: get-deps
	./rebar compile

get-deps:
	./rebar get-deps

update-deps:
	./rebar update-deps

clean:
	./rebar clean

deep-clean:
	rm -fr deps/*/ebin/*

run: compile
	export docker_bin=cat
	erl -pa ebin -pa deps/*/ebin \
	-name leviathan_lib@127.0.0.1 \
	-setcookie ${cookie} \
	-config sys.config \
	-eval "{ok, _} = application:ensure_all_started(leviathan_lib)" \
	-eval "leviathan_utils:connect_to_dobby()"

test:
	./rebar eunit skip_deps=true

qc:
	./rebar qc

qc_copmile:
	./rebar qc compile_only=true

rebar:
	wget -c https://github.com/rebar/rebar/wiki/rebar
	chmod +x rebar
