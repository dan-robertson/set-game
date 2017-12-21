#|
  This file is a part of set-game project.
  Copyright (c) 2017 Dan Robertson (dan.robertson14@gmail.com)
|#

#|
  Author: Dan Robertson (dan.robertson14@gmail.com)
|#

(in-package :cl-user)
(defpackage set-game-asd
  (:use :cl :asdf))
(in-package :set-game-asd)

(defsystem set-game
  :version "0.1"
  :author "Dan Robertson"
  :license "GPL3"
  :depends-on (:hunchensocket
               :hunchentoot)
  :components ((:module "src"
                :components
                ((:file "set-game"))))
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
  :in-order-to ((test-op (test-op set-game-test))))
