#!/bin/bash

$1 --noinform --non-interactive --load $2 --eval "(local-compile)" --quit ${@:3}
