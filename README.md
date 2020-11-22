# What

`lazybones` is a half-hearted effort at a small HTTP route handling
framework for smallish dynamic sites, preferably those whose state
lives in the lisp process.

It is not at all 'field tested', does not compete with more thorougly
conceived frameworks, and is itself just a kind of wrapper around
clack.

# Basic Example

``` lisp
(defpackage #:hello-site
  (:use #:cl)
  (import-from #:lazybones
               #:defroute 
               #:http-ok
               #:http-err)
  (import-from #:fictitious-image-db-package
               #:get-image-ids
               #:lookup-image
               #:image-mimetype
               #:image-data))
               
(in-package :hello-site)

(defroute :get "/hello"
  (http-ok "text/html"
    (spinneret:with-html-string
      (:doctype)
      (:html :lang "en"
        (:head 
          (:meta :charset "utf-8")
          (:title "some images"))
        (:body 
          (:h1 "Hello")
          (:ul
              (dolist (img-id (get-image-ids))  ; assuming some function to get ids
                (:li (:img :src (format nil "/image/~a" img-id))))))))))
            
(defroute :get "/image/:id"
  (alexandria:if-let (found (lookup-image id)) ; assuming a funciton to lookup images
    (http-ok (image-mimetype found)            ; and to query properties
             (image-data found))               ; a byte vector
    (http-err 404 "Not Found")))
    

(start :port 5000) ; start the server 

```

Example code that would serve a page containing a list of images.

## Helpful Features

Inside of any handler there is a dynamically bound variable `*req*`
that holds the HTTP request being processed.

Using a form called `with-handler-preamble`, groups of handlers can be
defined that all perform some initialization / access control steps.

For example:

``` lisp

(with-handler-preamble 
    ((unless (authorized-request *req*)
       (http-err 401 "Unauthorized"))

     (make-database-connection))
  
  (defroute :post "/image/:id/edit" 
    ;; ... handle image post ...
    )

  (defroute :delete "/image/:id"
     ;; ... handle image delete ...
     ))

```
