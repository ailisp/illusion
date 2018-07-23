(defsystem illusion-test
  :author "Bo Yao <icerove@gmail.com>"
  :license  "MIT"
  :depends-on (:illusion :fiveam :split-sequence)
  :components ((:module "t"
                :serial t
                :components
                ((:file "illusion")
                 (:file "test")
                 (:file "test-indicator-preserve-case")
                 (:file "test-delete-paren-reader")
                 (:file "test-delete-all-paren-readers")))))
