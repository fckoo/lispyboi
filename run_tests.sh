#!/bin/bash
set -e

TESTFILES=lisp/test_*.lisp
STDLIB=lisp/stdlib.lisp


echo "Running tests"
for TEST in $TESTFILES ; do
    echo "test: $TEST"
    ./lispyboi "$STDLIB" "$TEST"
done
