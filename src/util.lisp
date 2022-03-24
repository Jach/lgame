(in-package #:lgame.util)

(annot:enable-annot-syntax)

(defun collect-dependencies-into (systems root-system)
  "Recursively enumerates dependencies of 'root-system,
   placing them into the given 'systems hash-table
   with mapping system-string-name -> system."
  (loop for dependency-spec in (append (asdf:system-depends-on root-system)
                                       (asdf:system-defsystem-depends-on root-system))
        for dependency = (asdf/find-component:resolve-dependency-spec nil dependency-spec)
        for dependency-name = (and dependency (asdf:component-name dependency))
        do (when (and dependency (null (gethash dependency-name systems)))
             (setf (gethash dependency-name systems) dependency)
             (collect-dependencies-into systems dependency))))

@export
(defun display-licenses (&optional (root-system-name "lgame"))
  "Displays the licenses for the 'root-system-name (default lgame) and all its
   sub-dependencies in alphabetical order.
   Also shows foreign libraries."
  (let ((systems (make-hash-table :test #'equal))
        (root-system (asdf:find-system root-system-name)))
    (setf (gethash root-system-name systems) root-system)
    (collect-dependencies-into systems root-system)

    (destructuring-bind (names max-length) (loop for k being the hash-keys in systems
                                                 collecting k into names
                                                 maximizing (length k) into max-length
                                                 finally (return (list names max-length)))
      (setf names (sort names #'string<))

      (format t "~&~VA    ~A" max-length "System Name" "License") ; table header
      (format t "~&~V{~A~:*~}    ~V{~A~:*~}" max-length '("-") (length "License") '("-")) ; dash/hyphen printing after table header
      ; should just make-string max-length :initial-element #\*
      (dolist (name names)
        (format t "~&~VA    ~A~%" max-length name (asdf:system-licence (gethash name systems))))))

  (let* ((libs (cffi:list-foreign-libraries :loaded-only nil))
         (max-length (loop for lib in libs maximize (length (format nil "~s" (cffi:foreign-library-name lib))))))
    (format t "~%~%~va    ~va    ~a    ~a~%" max-length "Foreign Library"  max-length "Full Symbol" "Loaded?" "File")
    (format t "~v{~a~:*~}~2:*    ~v{~a~:*~}    ~v{~a~:*~}    ~v{~a~:*~}~%" max-length '("-") (length "Loaded?") '("-") (length "File") '("-"))

    (dolist (lib libs)
      (format t "~VA~2:*    ~VS    ~VA    ~VA~%"
              max-length (cffi:foreign-library-name lib)
              (length "Loaded?") (if (cffi:foreign-library-loaded-p lib) "yes" "no")
              (length "File") (cffi:foreign-library-pathname lib)))))
