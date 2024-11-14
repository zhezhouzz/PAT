#!/bin/bash

p check -tc $1 -v -s $2 -explore | grep "<ErrorLog> Assertion Failed: "$3 | wc
