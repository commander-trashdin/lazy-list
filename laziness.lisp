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
;;this probably will be useful

(defun lazy-list (fst &key (snd (1+ fst)) lst)
  (let ((step (- snd fst)))
    (if lst
        (if (> fst lst)  ;;maybe better in terms of take n?
            nil
            (lazy-cons fst (lazy-list (+ fst step) :snd (+ snd step) :lst lst)))
        (lazy-cons fst (lazy-list (+ fst step) :snd (+ snd step))))))


;;; ----------------------------------------------------

(defun print-lazy-list (lazylist &optional (stream t))
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

;;; ----------------------------------------------------

(defun lazy-map (foo &rest lazylists)
  (if (notany #'emptyp lazylists)
    (lazy-cons (apply foo (mapcar #'lazy-car lazylists))
               (apply #'lazy-map foo (mapcar #'lazy-cdr lazylists)))))

;;; ----------------------------------------------------

(defun take (n lazylist)
  (labels ((%take (n lazylist acc)
              (if (plusp n)
                  (%take (1- n) (lazy-cdr lazylist) (cons (lazy-car lazylist) acc))
                  acc)))
    (reverse (%take n lazylist (list)))))

(defun take-while (pred lazylist)
  (labels ((%take-while (pred lazylist acc)
              (let ((candidate (lazy-car lazylist)))
                (if (funcall pred candidate)
                    (%take-while pred (lazy-cdr lazylist) (cons candidate acc))
                    acc))))
    (reverse (%take-while pred lazylist (list)))))
