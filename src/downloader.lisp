(in-package :image-downloader)

(declaim (type (member :interactive :skip-file :ignore-error)
               *checksum-error-response*))
(defparameter *checksum-error-response* :ignore-error
  "What to do in case of checksum error? :INTERACTIVE is for invoke
the debugger and is default, :SKIP-FILE skips an image with invalid
checksum and :IGNORE-ERROR ignores an error and saves the file")

(defparameter *ignore-types* nil
  "A list of ignored image types")

(defparameter *cookie-jar* (cl-cookie:make-cookie-jar)
  "Dexador's cookie jar")

(defparameter *proxy* nil
  "Proxy for HTTP requests")

(defun make-request (uri)
  "Make a request to server"
  (multiple-value-bind (body code)
      (dexador:get
       (with-output-to-string (stream)
         (puri:render-uri uri stream))
       :connect-timeout 10
       :proxy *proxy*
       :cookie-jar *cookie-jar*)
    (unless (= code 200)
      (error 'bad-response-code
             :code code
             :uri uri))
    body))

(defun make-thread (uri-string)
  "Return a thread guessed on URI string"
  (let* ((uri (puri:parse-uri uri-string))
         (host (puri:uri-host uri)))
    (make-instance
     (cond
       ((string= "boards.4chan.org" host) '4chan-thread)
       ((string= "2ch.hk" host) '2ch-thread)
       (t (error 'unknown-resource :uri uri-string)))
     :uri uri)))

(defun remove-types (files types)
  "Return files with given types from source list"
  (remove-if
   (lambda (file)
     (let ((type (pathname-type file)))
       (some (lambda (%type) (string= type %type)) types)))
   files :key #'image-name))

(defun try-restarts (restarts)
  "Invoke the first available restart from list"
  (mapc (lambda (restart)
          (if (find-restart restart)
              (invoke-restart restart)))
        restarts))

(defun handle-conditions (condition)
  "Handle conditions automatically or fall back to the debugger"
  (typecase condition
    (file-error
     ;; We already have this file downloaded
     (invoke-restart 'file-skip))
    (bad-checksum
     (ecase *checksum-error-response*
       (:interactive
        ;; Do not handle the condition
        t)
       (:skip-file
        (invoke-restart 'file-skip))
       (:ignore-error
        (continue))))
    (unknown-resource
     ;; Skip the thread and continue with a new one
     (invoke-restart 'thread-skip))
    ((or bad-response-code usocket:timeout-error)
     ;; Try to skip a file first
     (try-restarts '(file-skip thread-skip))))
  ;; No appropriate restart is found
  (princ condition *error-output*)
  (terpri *error-output*)
  (force-output *error-output*))

(defgeneric download-images (uri directory)
  (:documentation "Download images from a thread. URI is a desired
resource WWW address. All files will be saved to DIRECTORY + some
guessed name, based on the name of the thread or URI."))

(defmethod download-images (uri directory)
  (with-simple-restart (thread-skip "Skip downloading this thread")
    (let* ((thread (make-thread uri))
           (pathname (merge-pathnames (uiop:ensure-directory-pathname
                                       (directory-name thread))
                                      (truename directory)))
           (files (remove-types (image-sources thread) *ignore-types*)))
      (ensure-directories-exist pathname)
      (format t "Downloading total of ~d images~%" (length files))
      (mapc (lambda (image)
              (format t ".")
              (force-output)
              (let ((file-path (merge-pathnames (image-name image) pathname)))
                (tagbody retry
                   (restart-case
                       (with-open-file (output file-path
                                               :direction :output
                                               :if-does-not-exist :create
                                               :element-type '(unsigned-byte 8))
                         (setf (image-data image) (make-request (image-uri image)))
                         (check-image image)
                         (write-sequence (image-data image) output))
                     (file-skip ()
                       :report "Skip downloading file" ())
                     (file-retry ()
                       :report "Retry downloading file" (go retry))))))
            files)))
  (format t "~%")
  (values))

(defmethod download-images :around (uri directory)
  (handler-bind
      (((or file-error usocket:socket-error
            image-downloader-error)
         #'handle-conditions))
    (call-next-method)))
