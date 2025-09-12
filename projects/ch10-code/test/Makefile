.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = glurk

all: compile
	${ERL} -pa '/c/Users/ZBT7RX/erlang-book/projects/ch10-code/test' -s glurk start -s init stop

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
