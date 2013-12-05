(declaim (optimize (speed 0) (space 0) (debug 3)))

(in-package "CL-USER")

(require "COCOA")
(require "cl-opengl")
(require "cl-glu")
(require "cl-utilities")

(defparameter *animation-timer-delay* (/ 1.0d0 60.0d0))
(defparameter *obj-path* "Lego_Man.obj")
(defparameter *eye-vector* (vector 10.0 0.0 70.0))
(defparameter *center-vector* (vector 0.0 20.0 0.0))
(defparameter *light-vector* (vector 0.0 0.0 0.0 10.0))
(defparameter *light-ambient* (vector 1.0 0.0 1.0 1.0))
(defparameter *light-diffuse* (vector 1.0 1.0 1.0 1.0))
(defparameter *light-specular* (vector 1 0.7 0.7 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass objfile () 
  ((vertices :initform (make-array 100
                                   :adjustable t
                                   :fill-pointer 0))
   (normals :initform (make-array 100
                                  :adjustable t
                                  :fill-pointer 0))
   (texcoords :initform (make-array 100
                                    :adjustable t
                                    :fill-pointer 0))
   (faces :initform (make-array 100
                                :adjustable t
                                :fill-pointer 0))))

(defun push-float-vector (container str-args)
  "takes an array of strings to be converted to floats and pushed to vector"
  (let* ((args (mapcar #'read-from-string str-args)))
    (vector-push-extend (coerce args 'vector) container)))
  
(defun read-face-index (str)
  (if (<= (length str) 0) -1 (read-from-string str)))

(defmethod load-file ((obj objfile) filepath)
  (with-slots (vertices normals texcoords faces) obj
    (with-open-file (in filepath)
      (when in
        (loop for line = (read-line in nil) while line do 
             (let* ((space-split (cl-utilities:split-sequence #\Space line))
                    (cmd (if (eql (length space-split) 0) 
                             "" 
                             (first space-split))))
               (cond ((eql (length cmd) 0)
                      t)
                     ((string-equal "v" cmd)
                      (push-float-vector vertices (rest space-split)))
                     ((string-equal "vn" cmd)
                      (push-float-vector normals (rest space-split)))
                     ((string-equal "vt" cmd)
                      (push-float-vector texcoords (rest space-split)))
                     ((string-equal "f" cmd)
                      (let* ((string-bundles (rest space-split))
                             (string-points 
                              (mapcar 
                               (lambda (x) 
                                 (cl-utilities:split-sequence #\/ x)) 
                               string-bundles))
                             (points 
                              (mapcar 
                               (lambda (x)
                                 (coerce 
                                  (mapcar #'read-face-index x)
                                  'vector))
                               string-points)))
                        (vector-push-extend (coerce points 'vector) faces)))
                     ((string-equal "#" cmd) t)
                     ((string-equal "g" cmd)
                      (format t "group~%"))
                     ((string-equal "o" cmd)
                      (format t "object~%"))
                     ((string-equal "mtllib" cmd)
                      (format t "material lib~%"))
                     ((string-equal "usemtl" cmd)
                      (format t "use materia~%"))
                     (t
                      (format t "other:~a~%" cmd)))))))))
    
(defmethod draw ((obj objfile))
  (with-slots (vertices normals texcoords faces) obj
    (loop for face across faces do 
         (gl:begin :quads)
         (loop for indices across face do
              (when (>= (aref indices 1) 0)
                (let* ((index (- (aref indices 1) 1))
                       (vec (aref texcoords index)))
                  (gl:tex-coord (aref vec 0) (aref vec 1))))
              (let* ((index (- (aref indices 2) 1))
                     (vec (aref normals index)))
                (gl:normal (aref vec 0) (aref vec 1) (aref vec 2)))
              (let* ((index (- (aref indices 0) 1))
                     (vec (aref vertices index)))
                (gl:vertex (aref vec 0) (aref vec 1) (aref vec 2))))
         (gl:end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simple-gl-view (ns:ns-opengl-view)
  ((animation-timer :foreign-type :id :accessor simple-gl-view-animation-timer)
   obj)
  (:metaclass ns:+ns-object))

(objc:defmethod (#/dealloc :void) ((self simple-gl-view))
  (break)
  (with-slots (animation-timer) self 
    (#/invalidate animation-timer)
    (#/release animation-timer))
  (call-next-method))

(objc:defmethod (#/drawRect: :void) ((self simple-gl-view) (rect :<NSR>ect))
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

(objc:defmethod (#/prepareOpenGL :void) ((self simple-gl-view))
  (gl:shade-model :smooth)
  (gl:clear-color 0.0 0.0 0.0 0.5)
  (gl:clear-depth 1.0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:hint :perspective-correction-hint :nicest)
  (setf (slot-value self 'obj) (make-instance 'objfile))
  (load-file (slot-value self 'obj) *obj-path*)
  (gl:enable :lighting)
  (gl:enable :light0)
  (gl:light :light0 :position *light-vector*)
  (gl:light :light0 :diffuse *light-diffuse*)
  (gl:light :light0 :ambient *light-ambient*)
  (gl:light :light0 :specular *light-specular*)
  (gl:enable :cull-face)
  (format t "prepare~%"))

(objc:defmethod (#/mouseDown: :void) ((self simple-gl-view) Event)
  (call-next-method event))

(objc:defmethod (#/keyDown: :void) ((self simple-gl-view) event)
  (let* ((code (#/keyCode event)))
    (cond ((eql code 126) ;up
           (incf (aref *eye-vector* 2) 0.2))
          ((eql code 125) ;down
           (incf (aref *eye-vector* 2) -0.2))
          ((eql code 123) ;left
           t)
          ((eql code 124) ;left
           t))))
    
(objc:defmethod (#/acceptsFirstResponder :boolean) ((self simple-gl-view)) t)

(defun get-seconds ()
  (let* ((now (get-internal-real-time))
        (seconds (/ (coerce now 'double-float)
                    (coerce internal-time-units-per-second 'double-float))))
    seconds))

(objc:defmethod (#/doAnimation: :void) ((self simple-gl-view) timer)
  (declare (ignore timer))
  (#/setNeedsDisplay: self t))

(objc:defmethod (#/reshape :void) ((self simple-gl-view))
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

(objc:defmethod (#/startAnimating :void) ((self simple-gl-view))
  (with-slots (animation-timer) self
    (let* ((timer
            (#/retain (#/scheduledTimerWithTimeInterval:target:selector:userInfo:repeats:
                       ns:ns-timer 
                       *animation-timer-delay*
                       self 
                       (objc:@selector #/doAnimation:) 
                       +null-ptr+ 
                       t)))
           (runloop (objc:send ns:ns-run-loop 'current-run-loop)))
      (objc:send runloop 
                 :add-timer timer
                 :for-mode #@"NSDefaultRunLoopMode")
      (setf animation-timer timer))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-obj ()
  (with-open-file (in *obj-path*)
    (when in
      (loop for line = (read-line in nil) while line do 
           (let* ((space-split (cl-utilities:split-sequence #\Space line))
                 (cmd (if (eql (length space-split) 0) "" (first space-split))))
             (cond ((eql (length cmd) 0)
                    t)
                   ((string-equal "v" cmd)
                    (format t "vertex~%"))
                   ((string-equal "vn" cmd)
                    (format t "normal~%"))
                   ((string-equal "vt" cmd)
                    (format t "texcoord~%"))
                   ((string-equal "f" cmd)
                    (format t "face~%"))
                   ((string-equal "#" cmd)
                    (format t "comment~%"))
                   ((string-equal "g" cmd)
                    (format t "group~%"))
                   ((string-equal "o" cmd)
                    (format t "object~%"))
                   ((string-equal "mtllib" cmd)
                    (format t "material lib~%"))
                   ((string-equal "usemtl" cmd)
                    (format t "use materia~%"))
                   (t
                    (format t "other:~a~%" cmd))))))))
