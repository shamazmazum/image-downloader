(in-package :image-downloader)

(defun after-last-slash (string)
  "Get the last non-empty string separated by slashes"
  (find-if (lambda (s) (string/= s ""))
           (split-sequence #\/ string)
           :from-end t))

(defmethod directory-name ((resource resource))
  (after-last-slash (puri:uri-path
                     (resource-uri resource))))

(defmethod directory-name ((thread imageboard-thread))
  (format nil "~a-~d"
          (imageboard-thread-name thread)
          (imageboard-thread-id thread)))

(defmethod initialize-instance :after ((resource resource) &rest initargs)
  (declare (ignore initargs))
  (download-resource resource))

(defmethod download-resource ((resource html-resource))
  (setf (resource-body resource)
        (html-parse:parse-html
         (make-request (resource-uri resource)))))

(defmethod download-resource ((resource json-api-resource))
  (setf (resource-body resource)
        (cl-json:decode-json-from-string
         (make-request (resource-uri resource)))))

(defmethod check-image ((image image))
  (declare (ignore image))
  t)

(defmethod check-image ((image image-md5))
  (let ((actual-md5   (md5:md5sum-sequence (image-data image)))
        (expected-md5 (image-md5 image)))
    (when (not (equalp actual-md5 expected-md5))
      (cerror "Ignore checksum error and continue"
              'bad-checksum :uri (image-uri image)))))
