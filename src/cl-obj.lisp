#|
  This file is a part of cl-obj project.
|#

(in-package :cl-user)
;; (defpackage cl-obj
;;   (:use :cl))
;; (in-package :cl-obj)

(defun new-pixel-format (&rest attributes)
  ;; take a list of opengl pixel format attributes (enums and other
  ;; small ints), make an array (character array?), and create and
  ;; return an NSOpenGLPixelFormat
  (let* ((attribute-size 
          (ccl::foreign-size :<NSO>pen<GLP>ixel<F>ormat<A>ttribute :bytes))
         (nattributes (length attributes)))
    (ccl::%stack-block ((objc-attributes (* attribute-size (1+ nattributes))))
      (loop for i from 0 to nattributes
         for attribute in attributes do
           (setf (ccl:paref objc-attributes (:* :<NSO>pen<GLP>ixel<F>ormat<A>ttribute) i) attribute) ; <- autocoerced?
         finally (setf (ccl:paref objc-attributes (:* :<NSO>pen<GLP>ixel<F>ormat<A>ttribute) nattributes) 0)) ; <- objc nil = null ptr
      (make-instance ns:ns-opengl-pixel-format 
                     :with-attributes objc-attributes))))

(defun show-simple-gl ()
  (ns:with-ns-rect (frame 0 0 1024 768)
    (let* ((win (make-instance 'ns:ns-window
                               :with-content-rect frame
                               :style-mask (logior #$NSTitledWindowMask
                                                   #$NSClosableWindowMask
                                                   #$NSResizableWindowMask
                                                   #$NSMiniaturizableWindowMask)
                               :backing #$NSBackingStoreBuffered
                               :defer t))
           (view (make-instance 'simple-gl-view
                                :with-frame frame
                                :pixel-format (new-pixel-format 
                                        ;#$NSOpenGLPFADoubleBuffer
                                               #$NSOpenGLPFAAccelerated
                                               #$NSOpenGLPFAColorSize 32
                                               #$NSOpenGLPFADepthSize 32))))
      (#/setReleasedWhenClosed: win t)
      (#/setContentView: win view)
      (#/release view)
      (#/center win)
      (#/orderFront: win nil)
      (#/performSelectorOnMainThread:withObject:waitUntilDone: 
       view 
       (objc:@selector #/startAnimating)
       +null-ptr+
       nil))))
