(in-package :image-downloader)

(defclass 4chan-thread (imageboard-thread json-api-resource)
  ()
  (:documentation "A thread on 4chan.org"))

(defmethod download-resource ((thread 4chan-thread))
  (destructuring-bind+ (_ board _ thread-id …)
      (split-sequence #\/ (puri:uri-path (resource-uri thread))
                      :test #'string=)
    (setf (imageboard-board thread)     board
          (imageboard-thread-id thread) thread-id
          (resource-uri thread)
          (make-instance 'puri:uri
                         :scheme :https
                         :host "a.4cdn.org"
                         :path (format nil "/~a/thread/~d.json" board thread-id))))
  (call-next-method))

(defmethod download-resource :after ((thread 4chan-thread))
  (let ((first-post (cadr (find :posts (resource-body thread) :key #'car))))
    (setf (imageboard-thread-name thread)
          (cdr (assoc :semantic--url first-post)))))

(defmethod image-sources ((thread 4chan-thread))
  (let (result)
    (mapc
     (lambda (post)
       (let ((tim (cdr (assoc :tim post)))
             (ext (cdr (assoc :ext post))))
         (if (and tim ext)
             (push
              (make-instance 'image-md5
                             :uri (make-instance 'puri:uri
                                                 :scheme :https
                                                 :host "i.4cdn.org"
                                                 :path (format nil "/~a/~d~a"
                                                               (imageboard-board thread) tim ext))
                             :name (make-pathname :name (format nil "~d" tim)
                                                  :type (subseq ext 1))
                             :md5 (with-input-from-string (in (cdr (assoc :md-5 post)))
                                    (s-base64:decode-base64-bytes in)))
              result))))
     (cdr (assoc :posts (resource-body thread))))
    result))
