(in-package :illusion.test)

(in-readtable :illusion-readtable)

(in-suite indicator-preserve-case)

(set-indicator-mode :preserve-case)

(test commonqt-methods
  (is (equal (list 'OPTIMIZED-CALL T 'PAINTER "setBrush" "Brush name")
             (read-from-string "(setBrush painter \"Brush name\")")))
  (is (equal '(if (some-func a)
               (aa)
               (bb))
             (read-from-string "(if (some-func a)
               (aa)
               (bb))"))))

(run! 'indicator-preserve-case)
(set-indicator-mode :standard)
