(in-package :cl-user)
(defpackage illusion
  (:use :cl)
  (:import-from :named-readtables
                :defreadtable
                :in-readtable
                :find-readtable)
  (:import-from :alexandria
                :if-let
                :when-let))
(in-package :illusion)

(defstruct paren-reader name predicate reader)

(defvar *paren-readers* nil)

(defvar *in-list-p* nil)

(defvar +empty-list+ (make-symbol "EMPTY-LIST"))

(defun cl-read-list (stream)
  (funcall (get-macro-character #\( (find-readtable :standard))
           stream
           #\())

(defun cl-read-right-paren (stream)
  (funcall (get-macro-character #\) (find-readtable :standard))
           stream
           #\)))

(defun find-paren-reader (indicator)
  (when-let (paren-reader
             (find-if (lambda (paren-reader)
                        (funcall (paren-reader-predicate paren-reader) indicator))
                      *paren-readers*))
    (paren-reader-reader paren-reader)))

(defun paren-reader-name-matcher (name)
  (lambda (paren-reader)
    (eql (paren-reader-name paren-reader) name)))

(defun find-paren-reader-by-name (name)
  (find-if (paren-reader-name-matcher name)
           *paren-readers*))

(defun illusion-read-list (stream ignore)
  (declare (ignore ignore))
  (let* ((*in-list-p* t)
         (indicator (read stream)))
    (unless (eql indicator +empty-list+)
      (if-let (reader (find-paren-reader indicator))
        (funcall reader stream indicator)
        (cons indicator (cl-read-list stream))))))

(defun illusion-read-right-paren (stream right-paren)
  (if *in-list-p*
      +empty-list+
      (cl-read-right-paren stream)))

(defun set-paren-reader (name predicate reader)
  (if-let (paren-reader (find-paren-reader-by-name name))
    (setf (paren-reader-predicate) predicate
          (paren-reader-reader) reader)
    (push (make-paren-reader :name name :predicate predicate :reader reader)
          *paren-readers*)))

(defun delete-paren-reader (name)
  (setf *paren-readers* (delete-if (paren-reader-name-matcher name) *paren-readers*)))

(defreadtable illusion-readtable
  (:merge :standard)
  (:macro-char #\( #'illusion-read-list)
  (:macro-char #\) #'illusion-read-right-paren))
