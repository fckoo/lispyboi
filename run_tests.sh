#!/bin/bash
set -e

TESTFILES=lisp/test_*.lisp
STDLIB=lisp/stdlib.lisp
ASSERTS=lisp/asserts.lisp


echo "Running tests"
for TEST in $TESTFILES ; do
    echo "test: $TEST"
    ./lispyboi "$STDLIB" "$ASSERTS" "$TEST"
done
