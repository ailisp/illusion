(in-package :cl-user)
(defpackage illusion.test
  (:use :cl :illusion :fiveam)
  (:import-from :named-readtables
                :in-readtable)
  (:import-from :split-sequence
                :split-sequence))

(defpackage stub-cli
  (:use :cl)
  (:export :define-cli))

(defpackage stub-html
  (:use :cl)
  (:export :div :a :canvas :code))

(defpackage stub-commonqt
  (:use :cl)
  (:export :optimized-call))

(in-package :illusion.test)

(in-readtable :illusion-readtable)

(set-paren-reader :define-cli
                  (lambda (i)
                    (eql i 'stub-cli:define-cli))
                  (lambda (stream indicator)
                    (cons 'stub-cli:define-cli
                          (cons (read stream)
                                (with-reader-case :preserve
                                    (cl-read-list stream))))))

(set-paren-reader :html
                  (lambda (i)
                    (when (symbolp i)
                      (let ((name (symbol-name i)))
                        (when (find #\. name)
                          (let ((name-and-id (split-sequence #\. name)))
                            (multiple-value-bind (symbol access) (find-symbol (first name-and-id) :stub-html)
                              (eql access :external)))))))
                  (lambda (stream indicator)
                    (let ((name-and-id (split-sequence #\. (symbol-name indicator))))
                      (list* (find-symbol (first name-and-id) :stub-html)
                             :id (string-downcase (second name-and-id))
                             (cl-read-list stream)))))

(defun qt-symbol-p (sym)
  (and (symbolp sym)
       (find sym '(|new| |moveCenter| |isActive| |setBrush|))))

(set-paren-reader :commonqt
                  #'qt-symbol-p
                  (lambda (stream indicator)
                    (list* 'optimized-call t (read stream) (symbol-name indicator)
                           (cl-read-list stream))))
