#!/bin/bash

./run.sh FailureDetector &&
    ./run.sh BankServer &&
    ./run.sh EspressoMachine &&
    ./run.sh Simplified2PC &&
    ./run.sh Kermit2PCModel
