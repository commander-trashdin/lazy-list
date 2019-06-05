;;;; laziness.asd

(asdf:defsystem #:laziness
  :description "Lazy lists implementation"
  :author "Andrew <aun.sokolov@gmail.com>"
  :license  "GNU 3.0"
  :version "0.5"
  :serial t
  :components ((:file "package")
               (:file "laziness")))
