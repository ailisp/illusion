(in-package :illusion.test)

(in-readtable :illusion-readtable)

(in-suite delete-paren-reader)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (delete-all-paren-readers))

(test delete-all-paren-readers
  (is (equal (cons 'stub-cli:define-cli
                   '(cli-name
                     (v version "Show version")
                     (v verbose "Set verbose level")))
             (read-from-string "(stub-cli:define-cli cli-name
               (v version \"Show version\")
               (V verbose \"Set verbose level\"))"))))

(run! 'delete-all-paren-readers)
