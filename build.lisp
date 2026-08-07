;;;; build.lisp — Build driver for the secd-lisp executable.
;;;;
;;;; Copyright (C) 2026
;;;; License: GPL3
;;;;
;;;; Loads the secd-lisp system and produces the standalone CLI binary at
;;;; build/secd-lisp via ASDF program-op:
;;;;   sbcl --non-interactive --load build.lisp

(ql:quickload "secd-lisp")
(ensure-directories-exist #p"build/secd-lisp")
(asdf:make "secd-lisp")
