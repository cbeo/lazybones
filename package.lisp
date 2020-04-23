;;;; package.lisp

(defpackage #:lazybones
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let
                #:when-let*
                #:starts-with-subseq)

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

(defpackage #:lazybones.decoders
  (:use #:cl #:parzival)
  (:import-from #:lazybones
                #:add-decoder))
