;;;; package.lisp

(defpackage #:lazybones
  (:use #:cl #:alexandria #:iterate)
  (:export

   #:*body*
   #:*req*
   #:*resp-headers*
   #:add-decoder
   #:add-header
   #:defroute
   #:http-err
   #:http-ok
   #:reload
   #:start
   #:stop
   ))
