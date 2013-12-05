
(in-package :cl-user)

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
