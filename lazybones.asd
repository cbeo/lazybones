;;;; lazybones.asd

(asdf:defsystem #:lazybones
  :description "http route handling"
  :author "Colin Okay <cbeok@protonmail.com>"
  :license  "AGPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:clack
               #:jonathan
               #:alexandria
               #:split-sequence
               #:do-urlencode
               #:arrows
               #:parzival
               #:uiop
               #:cl-fad)
  :components ((:file "package")
               (:file "lazybones")
               (:file "fs-serve")
               (:file "decoders")))
