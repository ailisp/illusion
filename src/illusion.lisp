(in-package :cl-user)
(defpackage illusion
  (:use :cl)
  (:import-from :named-readtables
                :defreadtable
                :find-readtable)
  (:import-from :alexandria
                :if-let
                :when-let)
  (:import-from :let-over-lambda
                :defmacro!)
  (:export :set-paren-reader
           :delete-paren-reader
           :delete-all-paren-readers
           :set-indicator-mode
           :cl-read-list
           :with-reader-case))
(in-package :illusion)

(defstruct paren-reader name predicate reader)

(defvar *indicator-reader* #'read)

(defvar *indicator-fallback* #'identity)

(defvar *paren-readers* nil)

(defvar *in-list-p* nil)

(defvar +empty-list+ (gensym "EMPTY-LIST"))

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
         (indicator (funcall *indicator-reader* stream)))
    (unless (eql indicator +empty-list+)
      (if-let (reader (find-paren-reader indicator))
        (funcall reader stream indicator)
        (cons (funcall *indicator-fallback* indicator) (cl-read-list stream))))))

(defun illusion-read-right-paren (stream right-paren)
  (if *in-list-p*
      +empty-list+
      (cl-read-right-paren stream)))

(defun set-paren-reader (name predicate reader)
  (if-let (paren-reader (find-paren-reader-by-name name))
    (setf (paren-reader-predicate paren-reader) predicate
          (paren-reader-reader paren-reader) reader)
    (push (make-paren-reader :name name :predicate predicate :reader reader)
          *paren-readers*)))

(defun delete-paren-reader (name)
  (setf *paren-readers* (delete-if (paren-reader-name-matcher name) *paren-readers*)))

(defun delete-all-paren-readers ()
  (setf *paren-readers* nil))

(defmacro! with-reader-case (case &body body)
  `(let ((,g!previous-case (readtable-case *readtable*)))
     (setf (readtable-case *readtable*) ,case)
     (unwind-protect (progn ,@body)
       (setf (readtable-case *readtable*) ,g!previous-case))))

(defun read-indicator-preserve-case (stream)
  (with-reader-case :preserve (read stream)))

(defun fallback-indicator-upcase (indicator)
  (if (and (symbolp indicator))
      (intern (string-upcase indicator) (symbol-package indicator))
      indicator))

(defun funcallable-p (name)
  (or (functionp name)
      (and (symbolp name)
           (fboundp name))))

(defun set-indicator-mode (mode)
  (ecase (type-of mode)
    ((keyword)
     (ecase mode
       (:preserve-case (setf *indicator-reader* #'read-indicator-preserve-case
                             *indicator-fallback* #'fallback-indicator-upcase))
       (:standard (setf *indicator-reader* #'read
                        *indicator-fallback* #'identity))))
    ((cons)
     (assert (and (funcallable-p (car mode)) (funcallable-p (cdr mode))))
     (setf *indicator-reader* (car mode)
           *indicator-fallback* (cdr mode)))))

(defreadtable :illusion-readtable
  (:merge :standard)
  (:macro-char #\( #'illusion-read-list)
  (:macro-char #\) #'illusion-read-right-paren))
