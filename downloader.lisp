(in-package :image-downloader)

(define-condition image-downloader-error ()
  ((uri  :initarg :uri :reader bad-uri)))

(define-condition bad-response-code (image-downloader-error)
  ((code :initarg :code :reader bad-code))
  (:report (lambda (c s)
             (format s "Bad response when reading URI ~a: ~d"
                     (bad-uri c)
                     (bad-code c)))))

(define-condition unknown-resource (image-downloader-error) ()
  (:report (lambda (c s)
             (format s "Do not know how to download from ~a"
                     (bad-uri c)))))

(defclass imageboard-thread ()
  ((uri  :reader thread-uri
         :initarg :uri
         :initform (error "You must specify an URI"))
   (body :accessor thread-body))
  (:documentation "Generic imageboard thread class"))

(defgeneric directory-name (thread)
  (:documentation "Guess directory name for saved files from thread name"))
(defgeneric image-sources (thread)
  (:documentation "Get images sources and their names"))

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

(defun after-last-slash (path)
  "Get string after the last slash symbol or between the two latest,
 if there is nothing after the last"
  (let* ((length (length path))
         (last-slash (char= (elt path (1- length)) #\/))
         (search-length
          (if last-slash
              (1- length) length))
         (slash-position (position #\/ path :from-end t :end search-length)))
  (subseq path (1+ slash-position) (if last-slash (1- length)))))

(defun make-request (uri)
  "Make a request to server"
  (multiple-value-bind (body code)
      (drakma:http-request uri :connection-timeout 10)
    (if (/= code 200)
        (error 'bad-response-code :code code :uri uri))
    body))

(defmethod directory-name ((thread imageboard-thread))
  (after-last-slash (puri:uri-path
                     (thread-uri thread))))

(defmethod image-sources :before ((thread imageboard-thread))
  (setf (thread-body thread)
        (html-parse:parse-html (make-request (thread-uri thread)))))

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
       (t (error 'unknown-resource :uri uri-string)))
     :uri uri)))

(defun download-images% (uri directory)
  "Download images from a thread (without error handling)"
  (with-simple-restart (toplevel-skip "Skip downloading thread")
    (let* ((thread (make-thread uri))
           (pathname (get-directory-pathname directory (directory-name thread))))
      (let ((files (image-sources thread)))
        (ensure-directories-exist pathname)
        (format t "Downloading total of ~d images~%" (length files))
        (mapc (lambda (file)
                (format t ".")
                (force-output)
                (destructuring-bind (uri . name) file
                  (with-simple-restart (file-skip "Skip downloading file")
                    (let ((file-path (merge-pathnames name pathname)))
                      (if (not (open file-path :direction :probe))
                          (let ((data (make-request uri)))
                            (with-open-file (output file-path
                                                    :direction :output
                                                    :if-does-not-exist :create
                                                    :element-type '(unsigned-byte 8))
                              (write-sequence data output))))))))
              files))))
  (format t "~%")
  t)

(defun download-images (uri directory)
  "Download images from a thread. URI is a desired resource WWW address.
 All files will be saved to DIRECTORY + some guessed name, based on the
 name of the thread or URI."
  (handler-bind
      ;; unrecoverable error
      (((or file-error unknown-resource)
        (lambda (c)
          (princ c)
          (invoke-restart 'toplevel-skip)))
       ;; possibly recoverable error
       ((or bad-response-code usocket:timeout-error)
        (lambda (c)
          (princ c)
          (if (find-restart 'file-skip)
              (invoke-restart 'file-skip)
              (invoke-restart 'toplevel-skip)))))
    (download-images% uri directory)))
