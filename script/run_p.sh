#!/bin/bash

p check -v -s $1 -explore | grep "<ErrorLog> Assertion Failed: "$2 | wc
