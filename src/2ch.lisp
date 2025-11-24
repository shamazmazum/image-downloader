(in-package :image-downloader)

(defun md5string=>vector (string)
  (apply #'vector
         (loop for i below 32 by 2 collect
              (parse-integer string :start i :end (+ i 2) :radix 16))))

(defclass 2ch-thread (imageboard-thread json-api-resource) ())

(defun set-2ch-userauth-code (code)
  (declare (type string code))
  (setq *cookie-jar*
        (cl-cookie:merge-cookies
         *cookie-jar*
         (list
          (cl-cookie:make-cookie
           :name   "ageallow"
           :value  "1"
           :domain "2ch.org")
          (cl-cookie:make-cookie
           :name   "usercode_auth"
           :value   code
           :domain "2ch.org")))))

(defmethod download-resource ((thread 2ch-thread))
  (let ((old-uri (puri:render-uri (resource-uri thread) nil)))
    (setf (resource-uri thread)
          (puri:parse-uri
           (replace old-uri ".json" :start1 (search ".html" old-uri)))))
  (call-next-method))

(defmethod download-resource :after ((thread 2ch-thread))
  (let ((body (resource-body thread)))
    (setf (imageboard-board thread)
          (alex:assoc-value (alex:assoc-value body :board) :id)
          (imageboard-thread-id thread)
          (alex:assoc-value body :current--thread)
          (imageboard-thread-name thread)
          (imageboard-board thread))))

(defmethod image-sources ((thread 2ch-thread))
  (let ((posts (alex:assoc-value (cadr (assoc :threads (resource-body thread))) :posts))
        result)
    (mapc
     (lambda (post)
       (mapc
        (lambda (file)
          (unless (assoc :sticker file)
            (push
             (make-instance 'image-md5
                            :uri (make-instance 'puri:uri
                                                :scheme :https
                                                :host "2ch.org"
                                                :path (alex:assoc-value file :path))
                            :name (pathname (alex:assoc-value file :name))
                            :md5 (md5string=>vector (alex:assoc-value file :md-5)))
             result)))
        (alex:assoc-value post :files)))
     posts)
    result))
