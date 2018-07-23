(in-package :illusion.test)

(in-readtable :illusion-readtable)

(in-suite delete-paren-reader)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (delete-paren-reader :html))

(test delete-single-paren-reader
  (is (equal '(div.main)
             (read-from-string "(div.main)"))))

(run! 'delete-single-paren-reader)

(in-suite default)
(run! 'case-sensitive-cmd-line)
