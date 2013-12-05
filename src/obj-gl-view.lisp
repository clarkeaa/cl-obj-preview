
(in-package :cl-user)

(defparameter *animation-timer-delay* (/ 1.0d0 60.0d0))
(defparameter *eye-vector* (vector 10.0 0.0 70.0))
(defparameter *center-vector* (vector 0.0 20.0 0.0))
(defparameter *light-vector* (vector 0.0 0.0 0.0 10.0))
(defparameter *light-ambient* (vector 1.0 0.0 1.0 1.0))
(defparameter *light-diffuse* (vector 1.0 1.0 1.0 1.0))
(defparameter *light-specular* (vector 1 0.7 0.7 1))

(defclass obj-gl-view (ns:ns-opengl-view)
  ((animation-timer :foreign-type :id :accessor obj-gl-view-animation-timer)
   obj)
  (:metaclass ns:+ns-object))

(objc:defmethod (#/dealloc :void) ((self obj-gl-view))
  (break)
  (with-slots (animation-timer) self 
    (#/invalidate animation-timer)
    (#/release animation-timer))
  (call-next-method))

(objc:defmethod (#/drawRect: :void) ((self obj-gl-view) (rect :<NSR>ect))
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-identity)
  (glu:look-at (aref *eye-vector* 0)
               (aref *eye-vector* 1)
               (aref *eye-vector* 2)
               (aref *center-vector* 0)
               (aref *center-vector* 1)
               (aref *center-vector* 2)
               0.0
               1.0
               0.0)
  (gl:with-pushed-matrix 
    (gl:scale 5.0 5.0 5.0)
    (draw (slot-value self 'obj)))
  (gl:flush))

(objc:defmethod (#/prepareOpenGL :void) ((self obj-gl-view))
  (gl:shade-model :smooth)
  (gl:clear-color 0.0 0.0 0.0 0.5)
  (gl:clear-depth 1.0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:hint :perspective-correction-hint :nicest)
  (gl:enable :lighting)
  (gl:enable :light0)
  (gl:light :light0 :position *light-vector*)
  (gl:light :light0 :diffuse *light-diffuse*)
  (gl:light :light0 :ambient *light-ambient*)
  (gl:light :light0 :specular *light-specular*)
  (gl:enable :cull-face)
  (format t "prepare~%"))

(objc:defmethod (#/mouseDown: :void) ((self obj-gl-view) Event)
  (call-next-method event))

(objc:defmethod (#/keyDown: :void) ((self obj-gl-view) event)
  (let* ((code (#/keyCode event)))
    (cond ((eql code 126) ;up
           (incf (aref *eye-vector* 2) 0.2))
          ((eql code 125) ;down
           (incf (aref *eye-vector* 2) -0.2))
          ((eql code 123) ;left
           t)
          ((eql code 124) ;left
           t))))
    
(objc:defmethod (#/acceptsFirstResponder :boolean) ((self obj-gl-view)) t)

(defun get-seconds ()
  (let* ((now (get-internal-real-time))
        (seconds (/ (coerce now 'double-float)
                    (coerce internal-time-units-per-second 'double-float))))
    seconds))

(objc:defmethod (#/doAnimation: :void) ((self obj-gl-view) timer)
  (declare (ignore timer))
  (#/setNeedsDisplay: self t))

(objc:defmethod (#/reshape :void) ((self obj-gl-view))
  (let* ((frame (#/frame self))
         (width (ns:ns-rect-width frame))
         (height (ns:ns-rect-height frame)))
    (#/update (#/openGLContext self))
    (gl:viewport 0 0 width height)
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 45.0 (/ width height) 0.1 100.0)
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(objc:defmethod (#/startAnimating :void) ((self obj-gl-view))
  (with-slots (animation-timer) self
    (let* ((timer
            (#/retain (#/scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:
                       ns:ns-timer 
                       *animation-timer-delay*
                       self 
                       (objc:@selector #/doAnimation:) 
                       +null-ptr+ 
                       t)))
           (runloop (#/currentRunLoop ns:ns-run-loop)))
      (objc:send runloop 
                 :add-timer timer
                 :for-mode #@"NSDefaultRunLoopMode")
      (setf animation-timer timer))))
  
