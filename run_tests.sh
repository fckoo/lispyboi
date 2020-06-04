#!/bin/bash
set -e

TESTFILES=lisp/test_*.lisp

echo "Running tests"
for TEST in $TESTFILES ; do
    echo "test: $TEST"
    ./lispyboi "$TEST"
done
