;;;; package.lisp

(defpackage #:lazybones
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let
                #:when-let*
                #:read-file-into-string
                #:read-file-into-byte-vector
                #:starts-with-subseq)
  (:import-from #:split-sequence #:split-sequence)

  (:nicknames :lzb)
  (:export
   #:*body*
   #:*req*
   #:*resp-headers*
   #:*logging-p*
   #:*logging-stream*
   #:*fallback-response-mimetype*
   #:add-decoder
   #:add-header
   #:serve-directory
   #:register-file-handler-config
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


(defpackage #:lazybones.fs-serve
  (:use #:cl)
  (:import-from #:lazybones #:register-file-handler-config)
  (:import-from #:alexandria
                #:read-file-into-string
                #:read-file-into-byte-vector))
