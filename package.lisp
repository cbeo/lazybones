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
   #:with-handler-preamble
   ))

(defpackage #:lazybones.decoders
  (:use #:cl #:parzival)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:arrows
                #:->>
                #:as->*)
  (:import-from #:do-urlencode
                #:urldecode)
  (:import-from #:lazybones
                #:add-decoder))
