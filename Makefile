ERL := erl -pa deps/fd_server/ebin -pa ebin +Bc +K true -smp enable -boot start_sasl -sname dns -setcookie dns -s crypto -s inets

all:
	rebar get-deps && rebar compile

clean:
	rebar clean

build_plt: all
	rebar skip_deps=true build-plt

analyze: all
	rebar skip_deps=true dialyze

doc: all
	rebar skip_deps=true doc

xref: all
	rebar skip_deps=true xref

shell: all
	${ERL}

run: all
	${ERL} -s eganglia
