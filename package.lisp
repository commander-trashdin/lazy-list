;;;; package.lisp

(defpackage #:laziness
  (:use #:cl)
  (:export #:emptyp
           #:lazy-car #:lazy-cdr
           #:lazy-cons
           #:lazy-list
           #:coerce-to-list
           #:foldl #:findl #:scanl
           #:lazy-map
           #:replicate
           #:take
           #:take-while
           #:drop
           #:drop-while
           #:iterate
           #:cycle
           #:lazy-nth
           #:filter
           #:concat
           #:%cartesian))
