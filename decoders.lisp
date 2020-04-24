
(in-package #:lazybones.decoders)

;;; HELPERS

(defun read-body-to-string (stream content-length)
  "Reads CONTENT-LENGTH characters from STREAM and returns a string."
  (let ((string (make-string content-length)))
    (read-sequence string stream)
    string))


(defun binary-content-p (content-type)
  (or (alexandria:starts-with-subseq "image" content-type)
      (alexandria:starts-with-subseq "audio" content-type)
      (and (alexandria:starts-with-subseq "application" content-type)
           (not (equal content-type "application/json")))
      (alexandria:starts-with-subseq "video" content-type)))


(defun butlast-to-string (res)
  (map 'string 'identity (butlast res)))

(defun make-keyword (str)
  (read-from-string (format nil ":~a" str)))

(defun write-binary-to-tmp-file (body)
  (cl-fad:with-output-to-temporary-file (out-file :element-type '(unsigned-byte 8))
    (loop :for char :across body :do (write-byte (char-int char) out-file))))

;;; PLAIN TEXT DECODER

(defun decode-plain-text (stream content-type content-length)
  (declare (ignore content-type))
  (read-body-to-string stream content-length))

(add-decoder "text/plain" #'decode-plain-text)


;;; JSON DECODER

(defun decode-json-body (stream content-type content-length)
  "Reads LEN characters from stream and decodes them as JSON, returning a PLIST"
  (declare (ignore content-type))
  (jonathan:parse (read-body-to-string stream content-length)))

(add-decoder "application/json" #'decode-json-body)


;;; MULTIPART/FORM-DATA DECODER

(<<def <crlf< (<<and (<<char #\Return)
                     (<<char #\Newline))
       "Matches the standard CRLF line ending used by HTTP")


(<<def <word-plus<
       (<<to-string (<<+  (<<or <alphanum< (<<any-char "-_/. "))))
       "parses a word like foo-bar or foo-bar-zoo4")


(<<def <key-equal-val<
       (<<let ((key (<<and <whitespace< <word-plus<))
               (val (<<and (<<char #\=)
                           (<<char-brackets #\" <word-plus< #\"))))
              (<<result (list (make-keyword key) val)))
       "Parses strings that look like foo-bar=\"goo\" and returns a list (:foo-bar \"goo\")")


(<<def <multipart-header-content-disposition<
       (<<map
        (lambda (pairs)  (apply 'append pairs))
        (<<and (<<string "Content-Disposition: form-data; ")
               (<<sep-by <key-equal-val<  (<<string "; "))))
       
       "Parses a Content-Disposition header in a multipart/form-data block. 
Returns a PLIST with one property, the value of which is also a PLIST.

E.g.      Content-Disposition: form-data; name=\"file\"; filename=\"mypic.png\" 
becomes   (:content-disposition (:name \"file\" :filename \"mypic.png\"))")

(<<def <multipart-header<
       (<<let ((header <word-plus<)
               (value  (<<map #'butlast-to-string
                              (<<and (<<char #\:)
                                     (<<until <crlf<)))))
              (<<result (list (make-keyword header)
                              (string-trim '(#\Space) value)))))




(defun <<multipart/form-data-part (stop-seq)
  (<<let ((disp (<<and (<<? <crlf<)
                       <multipart-header-content-disposition<))
          (headers (<<and <crlf<  (<<* <multipart-header<)))
          (body (<<and <crlf<
                       (<<map #'butlast-to-string
                              (<<until (<<and <crlf<
                                              (<<string stop-seq)))))))
         (<<result
          (let ((all-headers (nconc disp (apply 'append  headers))))
            (if (binary-content-p (getf all-headers :content-type))
                (nconc all-headers (list :body (write-binary-to-tmp-file body)))
                (nconc all-headers (list :body body)))))))



(defun <<multipart/form-data (boundary)
  (<<let ((parts (<<and (<<string boundary)
                        <crlf<
                        (<<+ (<<multipart/form-data-part boundary))))
          (_ending (<<and (<<string "--") <crlf<)) )
         (<<result parts)))

(defun decode-multipart/form-data (stream content-type content-length)
  (let* ((boundary (concatenate 'string
                                "--"
                                (second (split-sequence:split-sequence #\= content-type))))
         (stream (make-instance 'replay-streams:static-text-replay-stream
                                :text (read-body-to-string stream content-length)))) 
    (parse stream (<<multipart/form-data boundary))))

(add-decoder "multipart/form-data" #'decode-multipart/form-data)

;;; APPLICATION/X-WWW-FORM-URLENCODED

(defun decode-application/x-www-form-urlencoded (stream content-type content-length)
  (declare (ignore content-type))
  (->> (read-body-to-string stream content-length)
       (split-sequence #\&)
       (mapcar (lambda (s) (split-sequence #\= s)))
       (as->* pairs
              (loop
                 :for (key undecoded) :in pairs
                 :appending (list (make-keyword key)
                                  (urldecode undecoded :queryp t))))))

(add-decoder "application/x-www-form-urlencoded"
             #'decode-application/x-www-form-urlencoded)
