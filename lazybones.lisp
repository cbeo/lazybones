;;;; lazybones.lisp

(in-package #:lazybones)

;;; SPECIAL VARS

(defvar *handler* nil
  "Clack handler top-level handler.")

(defvar *routes* nil
  "Datastructure that maps routes to route handlers.")

(defvar *req* nil
  "A PLIST that bound by and available to route handlers.")

(defvar *body* nil
  "Holds body of a request, decoded according to known decoders. 

If no known decoder matches, holds a stream. 

Bound by route handlers for POST, PUT, and PATCH requests.")

(defvar *logging-p* nil
  "Set to T if you want ot log requests to the value of *logging-stream*.")

(defvar *logging-stream* t
  "Set to T (i.e the standard output) by default.")

(defvar *resp-headers* nil
  "A PLIST bound at the beginning of every response. Can be used to
  add additional headers to responses valid responses.")

(defvar *decoders* nil
  "An ALIST holding (mimetype . decoder) pairs. Add a decoder to this
  to customize decoding of POST and PUT bodies.")

(defvar *file-handler-configs* nil
  "An ALIST holding (EXTENSION MIMETYPE READER).  

EXTENSION is a  string, a file extension. 

MIMETYPE is a string, used for setting the Content-Type HTTP response
header for files with extension EXTENSION.  

READER is a function designator. The function should accept a OS path
and return either a string or a byte-vector.")

(defun register-file-handler-config (ext mimetype &optional (reader 'read-file-into-string))
  "Downcases both arguments, which are assumed to be strings. 

Adds the (EXT MIMETYPE READER) to the global file handler
registry. Used to determine the Content-Type when serving files whose
file extension is EXT.

READER is a function designator. The function should accept a path and
read that path from disk, returning either a string or a byte-vector."
  (if-let (entry (assoc ext *file-handler-configs* :test #'string-equal))
    (setf (second entry) (string-downcase mimetype)
          (third entry) reader)
    (push (list ext (string-downcase mimetype) reader)
          *file-handler-configs*)))

;; TODO raise a condition here in case of failure?
(defun get-file-handler-config (ext)
  "Looks up the mimetype for the file extention EXT. Returnes the
mimetype as a string, or NIL"
  (assoc ext *file-handler-configs* :test #'string-equal))

;;; HANDLER UTILITIES 

(defun add-header (key val)
  "Adds a header to the response headers. Can be used within a handler
definition."
  (setf (getf *resp-headers* key) val))


(defun add-decoder (mimetype decoder)
  "Adds or replaces a DECODER function for supplied MIMETYPE"
  (if-let ((decoder-pair (assoc mimetype *decoders* :test #'string-equal)))
    (setf (cdr decoder-pair) decoder)
    (push (cons mimetype decoder)
          *decoders*)))


(defun decode-body (stream content-type content-length)
  "Decodes the body according to the Content-Type header.

If no matching decoder is found in the *DECODERS* ALIST, then the 
STREAM itself is returned unaltered.
"
  (if-let ((decoder (assoc content-type *decoders*
                           :test (lambda (ct key) (starts-with-subseq key ct)))))
    (funcall (cdr decoder) stream  content-type content-length)
    stream))


(defun content-length (content)
  "Utility for determining the Content-Length header for response bodies."
  (cond ((consp content)
         (reduce #'+ (mapcar #'length content)))
        (t (length content))))


(defun http-ok (content-type &rest content)
  "Utility function for creating an 200 HTTP response. 

CONTENT-TYPE is a string, a mimetype.

CONTENT is either a list of strings or a byte vector. It can be other
stuff but CLACK has abysmal documentation.

HTTP-OK will determine the content length of the content automatically.

Any headers currently contained in the *RESP-HEADERS* ALIST will be
included in the response.

The function symbol HTTP-OK also has a different meaning when used
within the body of a DEFROUTE form.  There it will early escape from
the route handler with the value as described above. 

E.G. Consider the form 

(http-ok \"text/plain\" \"OK\")  

Outsidef of a DEFROUTE this returns the list

(200 (:CONTENT-TYPE \"text/plain\" :CONTENT-LENGTH 2) (\"OK\"))

But inside of a DEFROUTE, the same form would be equivalent to something like

(return-from #HANDERL123 
  (apply #'http-ok \"text/plain\" (\"OK\")))

where #HANDERL123 is a block label unique to the handler.
"
  (when (typep (car content) '(simple-array (unsigned-byte 8)))
    (setq content (car content)))
  (list 200
        (list* :content-type content-type
               :content-length (content-length content)
               *resp-headers*)
        content))

(defun serve-directory (root-path root-dir &key headers cache-p)
  "Adds handlers for every file in the directory tree with the root ROOT-DIR.

The if PATH is the file pathname relative to ROOT-DIR, then the route
added to serve the file located at PATH looks like ROOT-PATH/PATH. 

HEADERS and CACHE-P are passed to MAKE-FILE-HANDLER as the keyword
arguments of the same names.

If the appropriate mimetype cannot be determined for any file
encountered under the ROOT-DIR, then an error will be
signalled. Similarly, if a file reading function cannot be determined
an error will be signalled. See also REGISTER-FILE-HANDLER-CONFIG."
  (let ((prefix-len  (length (namestring root-dir)))
        (key-prefix (path-to-route-key :get root-path)))
    (uiop:collect-sub*directories
     root-dir
     (constantly t)
     (constantly t)
     (lambda (subdir)
       (dolist (file (uiop:directory-files subdir))
         (add-route
          (append key-prefix
                  (split-sequence
                   #\/
                   (subseq (namestring file) prefix-len)))
          (make-file-handler file :headers headers :cache-p cache-p)))))))

(defun make-file-handler
    (file &key
            mimetype
            file-reader
            headers
            cache-p)
  "Given a path to a file, returns a handler function for serving that
file.  If the file cannot be found on disk, an error will be raised
and the server will return 500.

If MIMETYPE is not specified, it will be determined from the file
extension. If it cannot be determined from the file extension, an
error will be raised.

FILE-READER names a function that reads file content from disk. It
should accept a file name and return either a string or a byte
vector. If it is not specified, it will be determined fromt he file
extension. If it cannot be determined from the file extension, an
error will be raised.

HEADERS is a PLIST of additional HTTP headers.  Content-Length need
not be included as it will be determined automatically.

CACHE-P determines whether or not the file is read from disk upon
every request.  By default files are not cached."
  (assert (probe-file file))
  (let* ((ext (pathname-type file))
         (config (get-file-handler-config ext))
         (mimetype (or mimetype
                       (second config)
                       (error "Unknown mimetype for file ~s~%" file)))
         (file-reader (or file-reader
                          (third config)
                          (error "Unknown file-reader for file ~s" file)))
         (content (if cache-p (funcall file-reader file)
                     (lambda () (funcall file-reader file)))))
    (lambda (*req*)
      (let ((*resp-headers* headers))
        (http-ok mimetype (if cache-p content (funcall content)))))))


(defun http-err (code text)
  (let ((resp (format nil "~a ~a" code text)))
    (list code
          (list :content-type "text/plain"
                :content-length (length resp))
          (list resp))))


(defun add-route (route-key route-handler)
  "A Helper, used by DEFROUTE. Adds or replaces a handler for a route.

ROUTE-KEY is of the form (METHOD . STRINGS) where METHOD is
one of :GET :POST :PUT :HEAD etc, and where STRINGS is a list of strings.

ROUTE-HANDLER is a function of several arguments. The first argument
always binds the special variable *REQ* to the current request PLIST.
Additional arguments are bound to variables that may appear in the
route key.  A string in STRINGS that starts with a colon will
correspond to a variable in the handler function.  This the value of
this variable is extracted from a request path and passed to the
handler function.

For example: (:GET \"persons\" \":id\" \"view\") matched against the
url path \"/persons/23/view\" would pass the value 23 to the route
handler, bound to the variable ID.
"
  (let ((found (assoc route-key *routes* :test #'equal)))
    (if found
        (setf (cdr found) route-handler)
        (push (cons route-key route-handler) *routes*))))

(defun path-var-p (str)
  "Returns T if STR is a string that looks like :foo, Nil otherwise."
  (and
   (stringp str)
   (plusp (length str))
   (eql #\: (aref str 0))))


(defun path-to-arglist (path-spec)
  "Parses a URL path and extracts any variables, returning a list of symbols.

E.g.: /foo/bar/:goo/zar/:moo  would result in  (GOO MOO)"

  (loop :for val :in (split-sequence:split-sequence #\/ path-spec)
       :when (path-var-p val) :collect (read-from-string (subseq val 1))))


(defmacro with-handler-preamble ((&rest preamble) &body route-defs)
  "Inserts PREAMBLE form into the beginning of each ROUTE-DEF, which
must be a valid DEFROUTE form.

WITH-HANDLER-PREAMBLE is useful for adding resource control or
preparation to big blocks of routes. E.g. ensuring authorization,
setting up variables, etc.
"
  (let ((transformed
         (loop
            :for (_ method path . handler-forms) :in route-defs
            :collect (list* 'lazybones:defroute
                            method
                            path
                            (append preamble handler-forms)))))
    `(progn ,@transformed)))

(defun path-to-route-key (method path)
  (cons method (split-sequence:split-sequence #\/ path)))


(defmacro defroute (method path &rest body)
  "Defines a new route handler.

Method is one of :GET :POST :PUT etc... 

PATH is a string representing a URL path. The PATH may contain
variable segmets, that start with a colon.

The new route is added to the currently defined routes and is
available for use.

If the METHOD has a body, then the defined route handler will
automatically decode the body according the the request's
Content-Length and Content-Type headers, which will then be bound to
*BODY* for the extent of the handler.

A special variable *RESP-HEADERS* is also bound to NIL at the start of
the handler, and can be used to add headers to a successful response. 

The request PLIST is boudn to *REQ* for the extent of the handler.

A handler is wrapped in an implicit block called
CURRENT-HANDLER, allowing for non-local exits via (RETURN-FROM CURRENT-HANDLER ...)
"
  (let* ((arglist (path-to-arglist path))
         (key (path-to-route-key method path))
         (block-label (gensym "HANDLER"))
         (body-block `(block ,block-label
                       (flet ((http-ok (content-type &rest content)
                                (return-from ,block-label
                                  (apply #'http-ok content-type content)))
                              (http-err (code text)
                                (return-from ,block-label
                                  (funcall #'http-err code text))))
                         ,@body))))
    
    (if (member method '(:post :put))
        `(add-route ',key
                    (lambda (*req* ,@arglist)
                      (let ((*body* (decode-body (getf *req* :raw-body)
                                                 (getf *req* :content-type)
                                                 (getf *req* :content-length)))
                            (*resp-headers*))
                        ,body-block)))
        `(add-route ',key
                    (lambda (*req* ,@arglist)
                      (let (*resp-headers*)
                        ,body-block))))))


(defun route-part-match-p (word1 word2)
  "A utility function, returns T if word1 and word2 unify as
non-variable URL path segments."
  (or (eql word1 word2)
      (and (stringp word1)
           (stringp word2)
           (string-equal word1 word2))))


(defun match-route-key (req-key route-key)
  "Compares a route keys extracted from an HTTP request path with an
already extant route key.  

Returns two values, a possible argument list to pass to the route
handler and a boolean indicating success"
  (if (not (= (length req-key) (length route-key)))
      (values nil nil)
      (let (args)
        (loop
           :for req-part :in req-key
           :for route-part :in route-key
           :do (cond
                 ((path-var-p route-part)
                  (push req-part args))

                 ((not (route-part-match-p req-part route-part))
                  (return-from match-route-key (values nil nil)))))

        (values (reverse  args) t))))


(defun lookup-route (req)
  "Looks up a route and returns two values. The first is a list of
arguments extracted from the request path. The second is the handler
function itself.  Both are NIL if the lookup failed to find a handler
for the request's path."
  (when-let* ((path   (getf req :path-info))
              (method (getf req :request-method))
              (key    (cons method (split-sequence:split-sequence #\/ path))))
    (loop :for (route-key . handler) :in *routes*
       :do (multiple-value-bind (args match-p) (match-route-key key route-key)
             (when match-p (return-from lookup-route (values args handler)))))
    ;; otherwise
    (values nil nil)))


(defun main-handler (*req*)
  (when *logging-p*
    (format *logging-stream* "~a~%" *req*))
  (handler-case
      (multiple-value-bind (args handler) (lookup-route *req*)
        (if handler
            (apply handler *req* args)
            (http-err 404 "Not Found")))
    (error (e)
      (print e *error-output* )
      (http-err 500 "Internal Server Error"))))



(defun start (&key (port 5000))
  (setf *handler* (clack:clackup #'main-handler :port port)))

(defun stop ()
  (when *handler*
    (clack:stop *handler*)))

(defun reload (&key (port 5000))
  (stop)
  (start :port port))
