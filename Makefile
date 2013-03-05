CWD=$(shell pwd)
NAME=$(shell basename ${CWD})
CT_LOG=${APP_DIR}/logs
REBAR?=${CWD}/rebar
COOKIE?=cookie_${NAME}
ERL?=/usr/bin/env erl
ERLARGS=-pa ebin -smp enable -name ${NODE} \
	-setcookie ${COOKIE} -boot start_sasl

all: clean compile

# Clean all.
clean:
	@${REBAR} clean

# Gets dependencies.
getdeps:
	@${REBAR} get-deps

# Compiles.
compile:
	@${REBAR} compile

# This one runs without a release.
shell: compile
	${ERL} ${ERLARGS}

