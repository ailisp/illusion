(defsystem illusion
  :author "Bo Yao <icerove@gmail.com>"
  :license "MIT"
  :version "0.1"
  :components ((:module "src"
                :serial t
                :components
                ((:file "illusion"))))
  :description "Customize and manage Lisp parens reader"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op illusion-test)))
  :depends-on (:named-readtables :alexandria :let-over-lambda))
