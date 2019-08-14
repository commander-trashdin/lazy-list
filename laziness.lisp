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

;;; Lazy-list ------------------------------------------ pretty self-explanatory functions here

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
  "Constructs a lazy list with first element being fst, then fst+step etc,
     second element is a way to dtermine a step."
  (let ((step (- snd fst)))
    (if lst
        (if (> fst lst)  ;;maybe better in terms of take n?
            nil
            (lazy-cons fst (lazy-list (+ fst step) :snd (+ snd step) :lst lst)))
        (%lazy-list fst (lambda (x) (+ step x))))))


;;; ----------------------------------------------------

(defun coerce-to-list (lazylist)
  "Coercsion to a normal CL list"
    (labels ((collect-lazy-list (lazylist acc)
                (if lazylist
                    (collect-lazy-list (lazy-cdr lazylist) (cons (lazy-car lazylist) acc))
                    acc)))
      (reverse (collect-lazy-list lazylist (list)))))
;;there are better ways to do this


;;; ----------------------------------------------------


(defun foldl (foo lazylist &key initial-val)
  "Same as reducing"
  (if initial-val
    (if lazylist
      (foldl foo (lazy-cdr lazylist) :initial-val (funcall foo initial-val (lazy-car lazylist)))
      initial-val)
    (foldl foo (lazy-cdr lazylist) :initial-val (lazy-car lazylist))))

(defun findl (pred lazylist)
  "Non-lazy finding"
  (if (funcall pred (lazy-car lazylist))
      (lazy-car lazylist)
      (findl pred (lazy-cdr lazylist))))


(defun scanl (foo lazylist &key initial-val)
  "Same as foldl, but leaves a traces of intermediate values"
  (if initial-val
    (lazy-cons initial-val (scanl foo (lazy-cdr lazylist) :initial-val (funcall foo initial-val (lazy-car lazylist))))
    (scanl foo (lazy-cdr lazylist) :initial-val (lazy-car lazylist))))


;;; ----------------------------------------------------

(defun lazy-map (foo &rest lazylists)
  "Maps over many lists"
  (if (notany #'emptyp lazylists)
    (lazy-cons (apply foo (mapcar #'lazy-car lazylists))
               (apply #'lazy-map foo (mapcar #'lazy-cdr lazylists)))))

(defun replicate (obj &optional (times nil))
  "List of repeating values"
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
  (if (emptyp lazylist)
      nil
      (let ((candidate (lazy-car lazylist)))
        (when (funcall pred candidate)
          (lazy-cons candidate (take-while pred (lazy-cdr lazylist)))))))

(defun filter (pred lazylist)
  (if (emptyp lazylist)
      nil
      (let ((candidate (lazy-car lazylist)))
        (if (funcall pred candidate)
            (lazy-cons candidate (filter pred (lazy-cdr lazylist)))
            (filter pred (lazy-cdr lazylist))))))


(defun concat (l1 l2)
  (if l1
      (lazy-cons (lazy-car l1)
                 (concat (lazy-cdr l1) l2))
      l2))

(defun %cartesian (fn &rest lists)  ;;for future list comprehension
  (when lists
    (labels ((recurse (lists values)
               (let ((rest (rest lists)))
                 (if rest
                     (labels ((inner-recurse (list)
                                (when list
                                  (concat
                                   (recurse rest (cons (lazy-car list) values))
                                   (inner-recurse (lazy-cdr list))))))
                       (inner-recurse (car lists)))
                     (labels ((yielding-recurse (list)
                                (when list
                                  (lazy-cons
                                   (apply fn (reverse (cons (lazy-car list) values)))
                                   (yielding-recurse (lazy-cdr list))))))
                       (yielding-recurse (car lists)))))))
      (recurse lists nil))))


(declaim (ftype (function (fixnum cons) t) lazy-nth)) ;;why did I do this? No idea.
(defun lazy-nth (n lazylist)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((nth-rec (n lazylist)
             (when (zerop n)
               (return-from nth-rec (lazy-car lazylist)))
             (nth-rec (1- n) (lazy-cdr lazylist))))
    (declare (ftype (function (fixnum cons) t) nth-rec))
    (nth-rec n lazylist)))
