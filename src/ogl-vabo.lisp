(in-package #:game)

(define-condition invalid-vbo-size (error)
  ((name   :initarg :name   :reader invalid-vbo-size/name)
   (length :initarg :length :reader invalid-vbo-size/length)
   (size   :initarg :size   :reader invalid-vbo-size/size))
  (:report (lambda (condition stream)
             (with-slots (name length size) condition
               (if name
                 (format stream "VBO ~a has invalid size ~a for length ~a" name size length)
                 (format stream "VBO has invalid size ~a for length ~a" size length))))))

(define-condition inconsistent-vbo-size (error)
  ((expected :initarg :expected :reader inconsistent-vbo-size/expected)
   (vbo      :initarg :vbo      :reader inconsistent-vbo-size/vbo))
  (:report (lambda (condition stream)
             (with-slots (expected vbo) condition
               (format stream "~a was expected to have ~a vertices" vbo expected)))))

(defstruct (vbo (:constructor make-vbo-raw))
  (name nil :type (or null symbol) :read-only t)
  (handle (error "Must provide HANDLE") :type fixnum :read-only t)
  (normalize (error "Must provide NORMALIZE") :type boolean :read-only t)
  (size (error "Must provide SIZE") :type fixnum :read-only t)
  (type (error "Must provide TYPE") :type keyword :read-only t)
  (vertex-count (error "Must provide VERTEX-COUNT") :type fixnum :read-only t))

(defun bind-vbo (vbo)
  (gl:bind-buffer :array-buffer (vbo-handle vbo)))

(defun make-vbo (size data &key name (normalize nil) (type :float) (usage :static-draw))
  (unless (vectorp data)
    (setf data (coerce data 'vector)))

  (let* ((length (length data))
         (arr    (gl:alloc-gl-array type length))
         (handle (gl:gen-buffer)))
    (unless (zerop (rem length size))
      (error 'invalid-vbo-size :name name :length length :size size))
    (gl:bind-buffer :array-buffer handle)
    (iter
      (for x in-vector data with-index i)
      (setf (gl:glaref arr i) x))
    (gl:buffer-data :array-buffer usage arr)
    (let ((free (lambda () (add-thunk (gl:delete-buffers (list handle)))))
          (vbo (make-vbo-raw :handle handle :name name :normalize normalize :size size :type type
                             :vertex-count (/ length size))))
      (finalize vbo free)
      vbo)))

(defstruct (vao (:constructor make-vao-raw))
  (handle (error "Must provide HANDLE") :type fixnum :read-only t)
  (vertex-count (error "Must provide VERTEX-COUNT") :type fixnum :read-only t)
  (vbo-bindings (error "Must provide VBO-BINDINGS") :type (vector (cons fixnum vbo))
                                                    :read-only t))

(defun make-vao (vbo-bindings)
  (when (zerop (length vbo-bindings))
    (error 'invalid-asset-field :field :vertices :why "VERTICES clause is empty"))

  (let* ((handle (gl:gen-vertex-array))
         (vertex-count (vbo-vertex-count (cdr (elt vbo-bindings 0)))))
    (gl:bind-vertex-array handle)
    (iter
      (for (loc . vbo) in-vector vbo-bindings)
      (unless (= vertex-count (vbo-vertex-count vbo))
        (error 'inconsistent-vbo-size :expected vertex-count :vbo vbo))
      (bind-vbo vbo)
      (gl:enable-vertex-attrib-array loc)
      (gl:vertex-attrib-pointer loc (vbo-size vbo) (vbo-type vbo) (vbo-normalize vbo) 0 0))
    (let ((free (lambda () (add-thunk (gl:delete-vertex-arrays (list handle)))))
          (vao (make-vao-raw :handle handle :vbo-bindings vbo-bindings :vertex-count vertex-count)))
      (finalize vao free)
      vao)))

(defun draw-vao (vao)
  (gl:bind-vertex-array (vao-handle vao))
  (gl:draw-arrays :triangles 0 (vao-vertex-count vao)))
