(defsystem illusion-test
  :author "Bo Yao <icerove@gmail.com>"
  :license  "MIT"
  :depends-on (:illusion)
  :components ((:module "t"
                :serial t
                :components
                ((:file "illusion")))))
