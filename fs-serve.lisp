(in-package #:lazybones.fs-serve)

(defun register-many (mime-prefix config &optional (reader 'read-file-into-string))
  (dolist (entry config)
    (if (stringp entry)
        (register-file-handler-config entry
                                      (concatenate 'string mime-prefix entry)
                                      reader)
        (let ((mtype (concatenate 'string mime-prefix (car entry))))
          (dolist (ext (cdr entry))
            (register-file-handler-config ext mtype reader))))))

(defparameter +image-mimetypes+
  '("png"
    "bmp"
    ("jpeg" "jpeg" "jpg" "jfif" "pjpeg" "pjp")
    "apng"
    "gif"
    ("x-icon" "ico" "cur")
    ("svg+xml" "svg")
    ("tiff" "tiff" "tif")
    "webp"
    )
  "Each entry in the list is either a string EXT that will be used to
  insert image/EXT mimetype for file extension EXT, or, is a
  list (IMGTYPE . EXTENSIONS) and will prodeuce a separate entry for
  each of the list EXTENSIONS")

(register-many "image/" +image-mimetypes+ 'read-file-into-byte-vector)

(defparameter +text-mimetypes+
  '(("plain" "txt" "csv" "tsv" "org" "md"
     "lisp" "py" "el" "c" "java" "scm" "rb" "rs" "cpp"
     "hx")
    "css"
    ("html" "html" "htm")
    ("javascript" "js")))

(register-many "text/" +text-mimetypes+)



