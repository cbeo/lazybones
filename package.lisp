;;;; package.lisp

(defpackage #:lazybones
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let
                #:when-let*)
  (:import-from #:iterate
                #:iter
                #:for
                #:in
                #:collect)
  (:nicknames :lzb)
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
