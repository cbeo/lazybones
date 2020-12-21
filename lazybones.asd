;;;; lazybones.asd

(asdf:defsystem #:lazybones
  :description "http route handling"
  :author "Colin Okay <cbeok@protonmail.com>"
  :license  "AGPLv3"
  :version "0.2.0"
  :serial t
  :depends-on (#:clack
               #:hunchentoot ;; temporary
               #:jonathan
               #:alexandria
               #:split-sequence
               #:do-urlencode
               #:arrows
               #:uiop
               #:cl-fad)
  :components ((:file "package")
               (:file "lazybones")
               (:file "fs-serve")
               (:file "decoders")))
