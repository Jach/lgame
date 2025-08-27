(in-package #:lgame.data-structures)

(defclass priority-queue ()
  ((backing-collection :accessor .backing-collection
                       :initarg :backing-collection
                       :initform :unsorted-vector
                       :type (member :pileup-heap :unsorted-vector)
                       :documentation "The backing data structure for this priority queue.
                                       (Expectation: I can drop this choice of backing collection and just use my unsorted vector until I need something better.
                                                     An initial quick measurement on a maze is that my unsorted vec is maybe 50-100 microseconds faster on average.)")
   (%storage :accessor .storage
             :documentation "The actual storage structure determined by backing-collection.")
   (size :accessor .size
         :initarg :size
         :initform 16
         :type fixnum
         :documentation "The initial number of elements that can be stored in the priority queue. Automatically grows if more elements are pushed in.")
   (comparison-fn :accessor .comparison-fn
                  :initarg :comparison-fn
                  :initform (lambda (item1 item2) (declare (ignorable item1 item2)) t)
                  :documentation "A comparison function or predicate that takes two items stored in the queue and returns true if item 1 has a higher priority than item 2."))

  (:documentation "A basic priority queue structure that allows for pushing new elements and poping them off in priority order, as determined by comparisons with the
                   given comparison function."))

(defmethod initialize-instance :after ((pq priority-queue) &key)
  (setf (.storage pq) (case (.backing-collection pq)
                          (:pileup-heap (pileup:make-heap (.comparison-fn pq)
                                                          :size (.size pq)))
                          (:unsorted-vector (make-array (.size pq) :adjustable t :fill-pointer 0))
                          (t (error "Must supply a valid :backing-collection")))))

(defmethod priority-queue-push ((pq priority-queue) item)
  "Pushes new item onto the priority queue"
  (case (.backing-collection pq)
    (:pileup-heap (pileup:heap-insert item (.storage pq)))
    (:unsorted-vector (vector-push-extend item (.storage pq)))))

(defun %top-index (pq)
  (let ((best-index 0))
    (loop for i from 1 below (length (.storage pq))
          when (funcall (.comparison-fn pq) (aref (.storage pq) i) (aref (.storage pq) best-index))
            do (setf best-index i))
    best-index))

(defmethod priority-queue-pop ((pq priority-queue))
  "Returns the element at the top of the queue and removes it from the queue."
  (assert (not (priority-queue-empty? pq)))
  (case (.backing-collection pq)
    (:pileup-heap (pileup:heap-pop (.storage pq)))
    (:unsorted-vector (let* ((top-index (%top-index pq)))
                        (rotatef (aref (.storage pq) top-index)
                                 (aref (.storage pq) (1- (length (.storage pq)))))
                        (vector-pop (.storage pq))))))

(defmethod priority-queue-top ((pq priority-queue))
  "Returns multiple values. When the queue is non-empty,
   the first value is the element at the top of the queue, and the second value T.
   When the queue is empty, the first value will be nil and the second value will be nil."
  (if (priority-queue-empty? pq)
      (values nil nil)
      (values (case (.backing-collection pq)
                (:pileup-heap (pileup:heap-top (.storage pq)))
                (:unsorted-vector (aref (.storage pq) (%top-index pq))))
              T)))

(defmethod priority-queue-empty? ((pq priority-queue))
  (case (.backing-collection pq)
    (:pileup-heap (pileup:heap-empty-p (.storage pq)))
    (:unsorted-vector (zerop (length (.storage pq))))))

