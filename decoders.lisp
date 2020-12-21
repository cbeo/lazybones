;;;; lazybones.decoders package.

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

(defun decode-text/plain (stream content-type content-length)
  (declare (ignore content-type))
  (read-body-to-string stream content-length))

(add-decoder "text/plain" #'decode-text/plain)


;;; JSON DECODER

(defun decode-application/json (stream content-type content-length)
  "Reads LEN characters from stream and decodes them as JSON, returning a PLIST"
  (declare (ignore content-type))
  (jonathan:parse (read-body-to-string stream content-length)))

(add-decoder "application/json" #'decode-application/json)


;;; MULTIPART/FORM-DATA DECODER

;; Temporarily justing the hunchentoot post-parameter feature
(add-decoder "multipart/form-data"
             (lambda (&rest ignore)
               (declare (ignore ignore))
               (loop :for (k . v) :in (hunchentoot:post-parameters*)
                     :when (and  (listp v) (= 3 (length v)))
                       :collect (list :name k
                                      :body (first v)
                                      :filename (second v)
                                      :content-type (third v))
                     :collect (list :name k :body v))))

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
