(in-package :image-downloader)

(define-condition image-downloader-error (error)
  ((uri  :initarg :uri
         :reader  bad-uri)))

(define-condition bad-response-code (image-downloader-error)
  ((code :initarg :code
         :reader  bad-code))
  (:report (lambda (c s)
             (format s "Bad response when reading URI ~a: ~d"
                     (bad-uri c)
                     (bad-code c)))))

(define-condition unknown-resource (image-downloader-error) ()
  (:report (lambda (c s)
             (format s "Do not know how to download from ~a"
                     (bad-uri c)))))

(define-condition bad-checksum (image-downloader-error)
  ((expected :initarg :expected
             :reader  expected-checksum)
   (actual   :initarg :actual
             :reader  actual-checksum))
  (:report (lambda (c s)
             (let ((*print-base* 16))
               (format s "Bad checksum in downloaded file: ~a:~% expected ~a,~% got ~a"
                       (bad-uri c)
                       (expected-checksum c)
                       (actual-checksum c))))))

(defclass resource ()
  ((uri  :accessor resource-uri
         :initarg :uri
         :initform (error "You must specify an URI"))
   (body :accessor resource-body))
  (:documentation "Generic resource class"))

(defclass imageboard-thread (resource)
  ((board       :accessor imageboard-board
                :type string)
   (thread-id   :accessor imageboard-thread-id
                :type (or unsigned-byte string))
   (thread-name :accessor imageboard-thread-name
                :type string)))

(defclass json-api-resource (resource)
  ())

(defclass image ()
  ((name :initarg :name
         :initform (error "Specify image name")
         :reader image-name)
   (uri  :initarg :uri
         :initform (error "Specify image URI")
         :reader image-uri)
   (data :initarg :data
         :initform #()
         :type simple-vector
         :accessor image-data))
  (:documentation "Class for representing images"))

(defclass image-md5 (image)
  ((md5 :initarg :md5
        :initform (error "Specify MD5 checksum")
        :reader image-md5))
  (:documentation "MD5 protected image"))

(defgeneric download-resource (resource)
  (:documentation "Dowload and parse thread"))
(defgeneric directory-name (resource)
  (:documentation "Guess directory name for saved files from thread name"))
(defgeneric image-sources (resource)
  (:documentation "Get images sources and their names"))
(defgeneric check-image (image)
  (:documentation "Check downloaded image for correctness.
Signals a condition BAD-CHECKSUM in case of an error."))

(defvar *ignored-extensions* nil)
(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))
(defparameter *ignore-checksum-errors* nil
  "Ignore checksum errors (for bad resources like 2ch.hk)")

(defun get-parameterized-tag (list tag &rest parameters)
  "Find a tag with parameters and its body in parsed HTML"
  (find-if (lambda (obj)
             (and (listp obj)
                  (listp (car obj))
                  (eq (caar obj) tag)
                  (every (lambda (parameter)
                           (string= (cdr parameter)
                                    (getf (cdar obj) (car parameter))))
                         parameters)))
           list))

(defun split-uri (string)
  (remove ""
          (split-sequence #\/ string)
          :test #'string=))

(defun after-last-slash (string)
  "Get string after the last slash symbol or between the two latest,
 if there is nothing after the last"
  (car (last (split-uri string))))

(defun make-request (uri)
  "Make a request to server"
  (multiple-value-bind (body code)
      (drakma:http-request uri
                           :connection-timeout 10
                           :cookie-jar *cookie-jar*)
    (if (/= code 200)
        (error 'bad-response-code :code code :uri uri))
    body))

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

(defmethod download-resource ((resource resource))
  (setf (resource-body resource)
        (html-parse:parse-html (make-request (resource-uri resource)))))

(defmethod download-resource ((resource json-api-resource))
  (pushnew '("application" . "json")
           drakma:*text-content-types*
           :test #'equalp)
  (setf (resource-body resource)
        (cl-json:decode-json-from-string
         (make-request (resource-uri resource)))))

(defmethod check-image ((image image))
  (declare (ignore image))
  t)

(defmethod check-image ((image image-md5))
  (let ((actual-md5   (md5:md5sum-sequence (image-data image)))
        (expected-md5 (image-md5 image)))
  (if (not (equalp actual-md5
                   expected-md5))
      (cerror "Ignore checksum error and continue"
              'bad-checksum
              :uri (image-uri image)
              :expected expected-md5
              :actual   actual-md5))))

(defun get-directory-pathname (path directory-name)
  "Construct a full pathname for saved images"
  (declare (type (or string pathname) path))
    (pathname
     (typecase path
       (string (concatenate 'string path "/" directory-name "/"))
       (pathname (make-pathname
                  :directory (append (pathname-directory path)
                                     (if (pathname-name path)
                                         (list (pathname-name path)))
                                     (list directory-name)))))))

(defun search-in-tree (tree predicate)
  "Recursively search in parsed HTML tree for something"
  (labels ((perform-search (acc tree)
             (if (listp tree)
                 (cond
                   ((funcall predicate tree) (cons tree acc))
                   ((cdr tree) (reduce #'perform-search (cdr tree)
                                       :initial-value acc))
                   (t acc))
                 acc)))
    (perform-search nil tree)))

(defun make-thread (uri-string)
  "Return a thread guessed on URI string"
  (let* ((uri (puri:parse-uri uri-string))
         (host (puri:uri-host uri)))
    (make-instance
     (cond
       ((string= "boards.4chan.org" host) '4chan-thread)
       ((string= "2ch.hk" host) '2ch-thread)
       ((string= "fishki.net" host) 'fishki-thread)
       ((string= "102chan.i2p" host) '102chan-thread)
       (t (error 'unknown-resource :uri uri-string)))
     :uri uri)))

(defun remove-extensions (files extensions)
  "Return files with given extensions from source list"
  (remove-if
   (lambda (file)
     (let ((extension (pathname-type file)))
       (some (lambda (ext) (string= ext extension)) extensions)))
   files :key #'image-name))

(defun download-images% (uri directory)
  "Download images from a thread (without error handling)"
  (with-simple-restart (thread-skip "Skip downloading this thread")
    (let* ((thread (make-thread uri))
           (pathname (get-directory-pathname directory (directory-name thread))))
      (let ((files (remove-extensions (image-sources thread) *ignored-extensions*)))
        (ensure-directories-exist pathname)
        (format t "Downloading total of ~d images~%" (length files))
        (mapc (lambda (image)
                (format t ".")
                (force-output)
                (let ((file-path (merge-pathnames (image-name image) pathname)))
                  (tagbody retry
                     (restart-case
                         (when (not (open file-path :direction :probe))
                           (setf (image-data image) (make-request (image-uri image)))
                           (check-image image)
                           (with-open-file (output file-path
                                                   :direction :output
                                                   :if-does-not-exist :create
                                                   :element-type '(unsigned-byte 8))
                             (write-sequence (image-data image) output)))
                       (file-skip ()
                         :report "Skip downloading file" ())
                       (file-retry ()
                         :report "Retry downloading file" (go retry))))))
              files))))
  (format t "~%")
  t)

(defun try-restarts (restarts)
  "Invoke the first available restart from list"
  (mapc (lambda (restart)
          (if (find-restart restart)
              (invoke-restart restart)))
        restarts))

(defun handle-conditions (condition)
  "Handle conditions automatically or fall back to the debugger"
  (typecase condition
    (bad-checksum
     (when *ignore-checksum-errors*
       (continue)))
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

(defun download-images (uri directory)
  "Download images from a thread. URI is a desired resource WWW address.
 All files will be saved to DIRECTORY + some guessed name, based on the
 name of the thread or URI."
  (handler-bind
      (((or file-error usocket:socket-error
            image-downloader-error)
        #'handle-conditions))
    (download-images% uri directory)))
