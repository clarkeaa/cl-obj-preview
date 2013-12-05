#|
  This file is a part of cl-obj project.
|#

(in-package :cl-user)
(defpackage cl-obj-asd
  (:use :cl :asdf))
(in-package :cl-obj-asd)

(defsystem cl-obj
  :version "0.1"
  :author ""
  :license ""
  :depends-on (:cl-opengl
               :cl-glu
               :cl-utilities)
  :components ((:module "src"
                :components
                ((:file "objfile")
                 (:file "simple-gl-view")
                 (:file "cl-obj"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op cl-obj-test))))
