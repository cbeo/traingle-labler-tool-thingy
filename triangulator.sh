#!/usr/bin/sh 

sbcl --load start.lisp & (sleep 3; emacsclient -c --eval '(slime-connect "localhost" 4006)')
