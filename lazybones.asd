;;;; lazybones.asd

(asdf:defsystem #:lazybones
  :description "Describe lazybones here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:clack #:jonathan #:iterate #:alexandria #:split-sequence)
  :components ((:file "package")
               (:file "lazybones")))
