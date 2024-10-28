#!/bin/bash

time dune exec -- bin/main.exe syn-timeout benchmarks/$1/task.ml output/$1 $2
