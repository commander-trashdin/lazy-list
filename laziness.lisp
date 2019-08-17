;;;; laziness.lisp

(in-package #:laziness)

(defstruct thunk
  (form nil :type (or function null))
  value)


(deftype natural () `(integer 0 4611686018427387903))

;;; ----------------------------------------------------
(defmethod print-object ((obj thunk) stream)
  (print-unreadable-object (obj stream :type t)
    (if (not (thunk-form obj))
        (prin1 (lazy-value obj) stream)
        (princ "UNREALIZED" stream))))

;;; ----------------------------------------------------
(declaim (ftype (function (thunk) boolean) thunk-realized-p))
(defun thunk-realized-p (thunk)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (not (thunk-form thunk)))

(declaim (ftype (function (thunk) boolean) emptyp))
(defun emptyp (lazylist)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (not (lazy-value lazylist)))
;;; ----------------------------------------------------

(defmacro lazy (&body form)
  `(make-thunk :form (lambda () ,@form)))

;;; ----------------------------------------------------

(declaim (ftype (function (thunk) T) lazy-value))
(defun lazy-value (thunk)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((form (thunk-form thunk)))
    (if (not form)
        (thunk-value thunk)
        (prog1
            (setf (thunk-value thunk) (funcall form))
          (setf (thunk-form thunk) nil)))))

;;; Lazy-list --------------------------pretty self-explanatory functions here
(declaim (ftype (function (thunk) T) lazy-cdr))
(defun lazy-cdr (lazylist)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (lazy-value (cdr (lazy-value lazylist))))


(declaim (ftype (function (thunk) T) lazy-car))
(defun lazy-car (lazylist)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (lazy-value (car (lazy-value lazylist))))
;;; ----------------------------------------------------

(defmacro lazy-cons (head tail)
  `(lazy (cons (lazy ,head) (lazy ,tail))))

;;; ----------------------------------------------------
(declaim (ftype (function (T function) thunk) %lazy-list))
(defun %lazy-list (fst foo)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (lazy-cons fst (%lazy-list (funcall foo fst) foo)))

(declaim (ftype (function (number &key (:snd number) (:lst number)) thunk) lazy-list))
(defun lazy-list (fst &key (snd (1+ fst)) lst)
  "Constructs a lazy list with first element being fst, then fst+step etc,
     second element is a way to dtermine a step."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((step (- snd fst)))
    (if lst
        (if (> fst lst) ;;maybe better in terms of take n?
            (lazy nil)
            (lazy-cons fst (lazy-list (+ fst step) :snd (+ snd step) :lst lst)))
        (%lazy-list fst (lambda (x) (+ step x))))))


;;; ----------------------------------------------------
(declaim (ftype (function (thunk) list) coerce-to-list))
(defun coerce-to-list (lazylist)
  "Coercsion to a normal CL list"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((collect-lazy-list (lazylist acc)
             (if (emptyp lazylist)
                 acc
                 (collect-lazy-list (lazy-cdr lazylist) (cons (lazy-car lazylist) acc)))))
    (declare (ftype (function (thunk list) list) collect-lazy-list))
    (reverse (collect-lazy-list lazylist (list)))))



;;; ----------------------------------------------------

(declaim (ftype (function (function thunk &key (:initial-val T)) T) foldl))
(defun foldl (foo lazylist &key initial-val)
  "Same as reducing"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if initial-val
      (if (emptyp lazylist)
          initial-val
          (foldl foo (lazy-cdr lazylist) :initial-val (funcall foo initial-val (lazy-car lazylist))))
      (foldl foo (lazy-cdr lazylist) :initial-val (lazy-car lazylist))))


(declaim (ftype (function (function thunk) T) findl))
(defun findl (pred lazylist)
  "Strict search"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (funcall pred (lazy-car lazylist))
      (lazy-car lazylist)
      (findl pred (lazy-cdr lazylist))))

(declaim (ftype (function (function thunk &key (:initial-val T)) T) scanl))
(defun scanl (foo lazylist &key initial-val)
  "Same as foldl, but leaves a trace of intermediate values"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if initial-val
      (lazy-cons initial-val (scanl foo (lazy-cdr lazylist) :initial-val (funcall foo initial-val (lazy-car lazylist))))
      (scanl foo (lazy-cdr lazylist) :initial-val (lazy-car lazylist))))


;;; ----------------------------------------------------

(defun lazy-map (foo &rest lazylists)
  "Maps over many lists"
  (if (notany #'emptyp lazylists)
    (lazy-cons (apply foo (mapcar #'lazy-car lazylists))
               (apply #'lazy-map foo (mapcar #'lazy-cdr lazylists)))))


(declaim (ftype (function (T &optional (or natural null)) thunk) replicate))
(defun replicate (obj &optional (times nil))
  "List of repeating values"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if times
    (if (plusp times)
        (lazy-cons obj (replicate obj (1- times)))
        (lazy nil))
    (%lazy-list obj (lambda (obj) obj))))


(declaim (ftype (function (T &optional function) thunk) iterate))
(defun iterate (obj &optional (pred #'identity))
  "Successive predicate applications -- always infinite list"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (function pred))
  (lazy-cons obj (iterate (funcall pred obj) pred)))


(declaim (ftype (function (list) thunk) cycle))
(defun cycle (ls)
  "Cycles the given list into infinity, as in '(1 2 3) -> (1 2 3 1 2 3 1 ...)"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((%encircle (lls)
             (declare (list lls))
             (if (null lls)
                 (%encircle ls)
                 (lazy-cons (car lls) (%encircle (cdr lls))))))
    (%encircle ls)))
;;; ----------------------------------------------------

(declaim (ftype (function (natural thunk) thunk) take))
(defun take (n lazylist)
  "Takes first n elements"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (plusp n)
      (lazy-cons (lazy-car lazylist) (take (1- n) (lazy-cdr lazylist)))
      (lazy nil)))


(declaim (ftype (function (natural thunk) thunk) drop))
(defun drop (n lazylist)
  "Drops first n elements"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (plusp n)
      (drop (1- n) (lazy-cdr lazylist))
      lazylist))

(declaim (ftype (function (function thunk) thunk) take-while))
(defun take-while (pred lazylist)
  "Takes elements until predicate holds"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (emptyp lazylist)
      (lazy nil)
      (let ((candidate (lazy-car lazylist)))
        (if (funcall pred candidate)
            (lazy-cons candidate (take-while pred (lazy-cdr lazylist)))
            (lazy nil)))))

(declaim (ftype (function (function thunk) thunk) drop-while))
(defun drop-while (pred lazylist)
  "Drops elements until predicate holds"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (emptyp lazylist)
      (lazy nil)
      (if (funcall pred (lazy-car lazylist))
          (drop-while pred (lazy-cdr lazylist))
          lazylist)))

(declaim (ftype (function (function thunk) thunk) filter))
(defun filter (pred lazylist)
  "Takes only the elements, for which predicate holds"
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (emptyp lazylist)
      (lazy nil)
      (let ((candidate (lazy-car lazylist)))
        (if (funcall pred candidate)
            (lazy-cons candidate (filter pred (lazy-cdr lazylist)))
            (filter pred (lazy-cdr lazylist))))))

(declaim (ftype (function ((or null thunk) (or null thunk)) (or thunk null)) concat))
(defun concat (l1 l2)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if l1
      (lazy-cons (lazy-car l1)
                 (concat (lazy-cdr l1) l2))
      l2))

(defun %cartesian (fn &rest lists)
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


(declaim (ftype (function (fixnum cons) T) lazy-nth))
(defun lazy-nth (n lazylist)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (labels ((nth-rec (n lazylist)
             (when (zerop n)
               (return-from nth-rec (lazy-car lazylist)))
             (nth-rec (1- n) (lazy-cdr lazylist))))
    (declare (ftype (function (fixnum cons) t) nth-rec))
    (nth-rec n lazylist)))


;;;---------------Development area-------------
#||(defmacro such (exp &body body))
  (labels ((generator ()
             exp)))
||#

(declaim (ftype (function (natural) thunk) primes-to))
(defun primes-to (n)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if (< n 2)
      (lazy nil)
      (labels ((filter-prime (ls)
                 (let ((p (lazy-car ls))
                       (rest (lazy-cdr ls)))
                   (declare (natural p) (thunk rest))
                   (lazy-cons p (filter-prime (filter (lambda (x)
                                                        (declare (natural x))
                                                        (/= 0 (mod x p)))
                                                      rest))))))
        (declare (ftype (function (thunk) thunk) filter-prime))
        (take-while (lambda (x)
                        (declare (natural x))
                        (<= x n)) (filter-prime (lazy-list 2))))))
