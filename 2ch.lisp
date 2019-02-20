(in-package :image-downloader)

(defclass 2ch-thread (imageboard-thread json-api-resource) ())

(defun set-2ch-userauth-code (code)
  (declare (type string code))
  (pushnew
   (make-instance 'drakma:cookie
                  :name "ageallow"
                  :value "1"
                  :domain "2ch.hk")
   (drakma:cookie-jar-cookies *cookie-jar*)
   :test #'drakma:cookie=)
  (pushnew
   (make-instance 'drakma:cookie
                  :name "usercode_auth"
                  :value code
                  :domain "2ch.hk")
   (drakma:cookie-jar-cookies *cookie-jar*)
   :test #'drakma:cookie=))

(defmethod download-resource ((thread 2ch-thread))
  (let ((old-uri (puri:render-uri (resource-uri thread) nil)))
    (setf (resource-uri thread)
          (puri:parse-uri
           (replace old-uri ".json" :start1 (search ".html" old-uri)))))
  (call-next-method))

(defmethod download-resource :after ((thread 2ch-thread))
  (let ((body (resource-body thread)))
    (setf (imageboard-board thread)
          (cdr (assoc :*board body))
          (imageboard-thread-id thread)
          (cdr (assoc :current--thread body))
          (imageboard-thread-name thread)
          (imageboard-board thread))))

(defmethod image-sources ((thread 2ch-thread))
  (let ((posts (cdr (assoc :posts (cadr (assoc :threads (resource-body thread))))))
        result)
    (mapc (lambda (post)
            (mapc (lambda (file)
                    (push
                     (cons
                      (make-instance 'puri:uri
                                     :scheme :https
                                     :host "2ch.hk"
                                     :path (cdr (assoc :path file)))
                      (pathname (cdr (assoc :name file))))
                     result))
                  (cdr (assoc :files post))))
          posts)
    result))
