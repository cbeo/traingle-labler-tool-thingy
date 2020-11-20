#!/usr/bin/sh 

sbcl --eval "(ql:quickload :triangulator)"  --eval "(triangulator::start-debug)" & (sleep 3; emacsclient -c --eval '(slime-connect "localhost" 4006)' )
