(in-package :illusion.test)

(in-readtable :illusion-readtable)

(def-suite default)
(def-suite indicator-preserve-case)
(def-suite delete-paren-reader)

(in-suite default)

(test corner-case
  (is (eql () (read-from-string "()")))
  (is (eql :reader-error
           (handler-case (read-from-string ")")
             (reader-error () :reader-error)))))

(test case-sensitive-cmd-line
  (is (equal (cons 'stub-cli:define-cli
                   '(cli-name
                     (|v| |version| "Show version")
                     (v |verbose| "Set verbose level")))
             (read-from-string "(stub-cli:define-cli cli-name
               (v version \"Show version\")
               (V verbose \"Set verbose level\"))")))
  (is (equal (list '|SOMETHING-ELSE| '(|A| |A| |a| |A| (|b| |B| |B| |B|)))
             (read-from-string "(something-else (a A \\a \\A (\\b \\B B b)))"))))

(test html-ids
  (is (equal (list 'stub-html:div :id "main")
             (read-from-string "(div#main)")))
  (is (equal (list 'div "something")
             (read-from-string "(div \"something\")")))
  (is (equal '(|SOMETHING-ELSE| \A \B 3 "a")
             (read-from-string "(something-else a b 3 \"a\")"))))

(run! 'default)
