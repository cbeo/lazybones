;;;; lazybones.lisp

(in-package #:lazybones)

(defvar *handler* nil
  "Clack handler top-level handler.")

(defvar *routes* nil
  "Datastructure tha tmaps routes to route handlers.")

(defvar *req* nil
  "A PLIST that bound by and available to route handlers.")

(defvar *body* nil
  "Holds body of a request, decoded according to known decoders. 

If no known decoder matches, holds a stream. 

Bound by route handlers for POST, PUT, and PATCH requests.")

(defvar *resp-headers* nil
  "A PLIST bound at the beginning of every response. Can be used to
  add additional headers to responses valid responses.")

(defvar *decoders* nil
  "An ALIST holding (mimetype . decoder) pairs. Add a decoder to this
  to customize decoding of POST and PUT bodies.")

(defun add-header (key val)
  "Adds a header to the response headers. Can be used within a handler
definition."
  (setf (getf *resp-headers* key) val))


(defun read-body-to-string (stream content-length)
  "Reads CONTENT-LENGTH characters from STREAM and returns a string."
  (let ((string (make-string content-length)))
    (read-sequence string stream)
    string))


(defun decode-json-body (stream len)
  "Reads LEN characters from stream and decodes them as JSON, returning a PLIST"
  (jonathan:parse (read-body-to-string stream len)))


(defun add-decoder (mimetype decoder)
  "Adds or replaces a DECODER function for supplied MIMETYPE"
  (if-let ((decoder-pair (assoc mimetype *decoders* :test #'string-equal)))
    (setf (cdr decoder-pair) decoder)
    (push (cons mimetype decoder)
          *decoders*)))

(add-decoder "application/json" #'decode-json-body)


(defun decode-body (stream content-type content-length)
  "Decodes the body according to the Content-Type header.

If no matching decoder is found in the *DECODERS* ALIST, then the 
STREAM itself is returned unaltered.
"
  (if-let ((decoder (assoc content-type *decoders* :test (lambda (ct key) (starts-with-subseq key ct)))))
    (funcall (cdr decoder) stream  content-length)
    stream))


(defun content-length (content)
  "Utility for determining the Content-Length header for response bodies."
  (cond ((consp content)
         (reduce #'+ (mapcar #'length content)))
        (t (length content))))


(defun http-ok (content-type &rest content)
  "Utility function for creating an 200 HTTP response. 

CONTENT-TYPE is a string, a mimetype.

CONTENT is a list of strings. It can be other stuff but CLACK has
abysmal documentation."
  (when (typep (car content) '(simple-array (unsigned-byte 8)))
    (setq content (car content)))
  (list 200
        (list* :content-type content-type
               :content-length (content-length content)
               *resp-headers*)
        content))


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

  (iter (for val in (split-sequence:split-sequence #\/ path-spec))
        (when (path-var-p val)
          (collect (read-from-string (subseq val 1))))))



(defmacro defroute (method path &rest body)
  "Defines a new route handler.

Method is one of :GET :POST :PUT etc... 

PATH is a string representing a URL path. The PATH may contain
variable segmets, that start with a colon.

The new route is added to the currently defined routes and is available for use.

If the METHOD has a body, then the defined route handler will
automatically decode the body according the the request's
Content-Length and Content-Type headers, which will then be bound to
*BODY* for the extent of the handler.

A special variable *RESP-HEADERS* is also bound to NIL at the start of
the handler, and can be used to add headers to a successful response. 

The request PLIST is boudn to *REQ* for the extent of the handler."
  (let ((arglist (path-to-arglist path))
        (key (cons method (split-sequence:split-sequence #\/ path))))
    (if (member method '(:post :put))
        `(add-route ',key
                    (lambda (*req* ,@arglist)
                      (let ((*body* (decode-body (getf *req* :raw-body)
                                                 (getf *req* :content-type)
                                                 (getf *req* :content-length)))
                            (*resp-headers* nil))
                        ,@body)))
        `(add-route ',key
                    (lambda (*req* ,@arglist)
                      (let (*resp-headers*) ,@body))))))


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
  (let (args)
    (loop
       :for req-part :in req-key
       :for route-part :in route-key
       :do (cond
             ((path-var-p route-part)
              (push req-part args))

             ((not (route-part-match-p req-part route-part))
              (return-from match-route-key (values nil nil)))))

    (values (reverse  args) t)))


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


(defun main-handler (req)
  (handler-case
      (multiple-value-bind (args handler) (lookup-route req)
        (if handler
            (apply handler req args)
            (http-err 404 "Not Found")))
    (error (e)
      (print e *error-output* )
      (http-err 500 "Internal Server Error"))))



(defun start ()
  (setf *handler* (clack:clackup #'main-handler)))

(defun stop ()
  (clack:stop *handler*))

(defun reload ()
  (stop)
  (start))
