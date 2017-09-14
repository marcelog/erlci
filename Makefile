CWD=$(shell pwd)
ROOT?=${CWD}
REBAR?=${ROOT}/rebar3.1

all: clean dialyzer xref

clean:
	${REBAR} clean

dialyzer:
	${REBAR} dialyzer

xref:
	${REBAR} xref