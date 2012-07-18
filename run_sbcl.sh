#!/bin/sh

sbcl --load ./biomine-tv --eval '(progn (biomine-tv::main "localhost" 7890))'

# DISPLAY=:0 sbcl --load tv-libs --eval '(sb-ext:save-lisp-and-die "tv.core" :purify t :compression t)'
