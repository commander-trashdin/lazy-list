;;;; laziness.lisp

(in-package #:laziness)

(defclass thunk ()
  ((form  :initarg :form)
   (value :initarg :value)))


;;; ----------------------------------------------------
;;;consider rewriting, not sure how this works
(defmethod print-object ((obj thunk) stream)
  (print-unreadable-object (obj stream :type t)
    (if (slot-boundp obj 'value)
        (prin1 (lazy-value obj) stream)
      (princ "UNREALIZED" stream))))

;;; ----------------------------------------------------

(defun thunk-realized-p (thunk)
  (slot-boundp thunk 'value))

(defun emptyp (lazylist)
  (not lazylist))
;;; ----------------------------------------------------

(defmacro lazy (&body form)
  `(make-instance 'thunk :form (lambda () ,@form)))

;;; ----------------------------------------------------

(defun lazy-value (thunk)
  (with-slots (form value)
      thunk
    (if (thunk-realized-p thunk)
        value
        (setf value (funcall form)))))

;;; Lazy-list --------------------------------------------------------------------

(defun lazy-cdr (lazylist)
  (lazy-value (cdr (lazy-value lazylist))))

(defun lazy-car (lazylist)
  (lazy-value (car (lazy-value lazylist))))
;;; ----------------------------------------------------

(defmacro lazy-cons (head tail)
  `(lazy (cons (lazy ,head) (lazy ,tail))))

;;; ----------------------------------------------------

(defun %lazy-list (fst foo)
  (lazy-cons fst (%lazy-list (funcall foo fst) foo)))


(defun lazy-list (fst &key (snd (1+ fst)) lst)
  (let ((step (- snd fst)))
    (if lst
        (if (> fst lst)  ;;maybe better in terms of take n?
            nil
            (lazy-cons fst (lazy-list (+ fst step) :snd (+ snd step) :lst lst)))
        (%lazy-list fst (lambda (x) (+ step x))))))


;;; ----------------------------------------------------

(defun coerce-to-list (lazylist &optional (stream t))
    (labels ((collect-lazy-list (lazylist acc)
                (if lazylist
                    (collect-lazy-list (lazy-cdr lazylist) (cons (lazy-car lazylist) acc))
                    acc)))
      (format stream "~s" (reverse (collect-lazy-list lazylist (list))))))
;;there are better ways to do this


;;; ----------------------------------------------------


(defun foldl (foo lazylist &key initial-val)
  (if initial-val
    (if lazylist
      (foldl foo (lazy-cdr lazylist) :initial-val (funcall foo initial-val (lazy-car lazylist)))
      initial-val)
    (foldl foo (lazy-cdr lazylist) :initial-val (lazy-car lazylist))))

(defun findl (pred lazylist)
  (if (funcall pred (lazy-car lazylist))
      (lazy-car lazylist)
      (findl pred (lazy-cdr lazylist))))


(defun scanl (foo lazylist &key initial-val)
  (if initial-val
    (lazy-cons initial-val (scanl foo (lazy-cdr lazylist) :initial-val (funcall foo initial-val (lazy-car lazylist))))
    (scanl foo (lazy-cdr lazylist) :initial-val (lazy-car lazylist))))


;;; ----------------------------------------------------

(defun lazy-map (foo &rest lazylists)
  (if (notany #'emptyp lazylists)
    (lazy-cons (apply foo (mapcar #'lazy-car lazylists))
               (apply #'lazy-map foo (mapcar #'lazy-cdr lazylists)))))

(defun replicate (obj &optional (times nil))
  (if times
    (if (plusp times)
        (lazy-cons obj (replicate obj (1- times)))
        nil)
    (%lazy-list obj (lambda (obj) obj))))


;;; ----------------------------------------------------

(defun take (n lazylist)
  (if (plusp n)
      (lazy-cons (lazy-car lazylist) (take (1- n) (lazy-cdr lazylist)))
      nil))

(defun take-while (pred lazylist)
  (let ((candidate (lazy-car lazylist)))
    (if (funcall pred candidate)
        (lazy-cons candidate (take-while pred (lazy-cdr lazylist)))
        nil)))

(defun lazy-nth (n lazylist)
  (if (plusp n)
      (lazy-nth (1- n) (lazy-cdr lazylist))
      (lazy-car lazylist)))
