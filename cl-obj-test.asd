#|
  This file is a part of cl-obj project.
|#

(in-package :cl-user)
(defpackage cl-obj-test-asd
  (:use :cl :asdf))
(in-package :cl-obj-test-asd)

(defsystem cl-obj-test
  :author ""
  :license ""
  :depends-on (:cl-obj
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "cl-obj"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
