;; Standalone downloader and building script.
;; To get a standalone application, run make instead of doing something with this file

(defpackage image-downloader-standalone
  (:use #:cl #:image-downloader #:sb-ext))

(defvar *executable-name*) ; to silence warnings

(in-package :image-downloader-standalone)

(defun get-argv ()
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl (error "Do not know how to get argv, ha-ha!"))

(defun print-usage-and-exit ()
  (format *error-output* "echo thread-uri | image-downloader [--proxy-host proxy-host] [--proxy-port proxy-port] <directory>~%")
  (sb-ext:exit))

(defun parse-args (args &key proxy-host proxy-port)
  (let ((key (first args))
        (value (second args)))
  (cond
    ((string= "--proxy-host" key)
     (parse-args (cddr args)
                 :proxy-host value
                 :proxy-port proxy-port))
    ((string= "--proxy-port" key)
     (parse-args (cddr args)
                 :proxy-host proxy-host
                 :proxy-port
                 (handler-case
                     (parse-integer value)
                   (parse-error () (print-usage-and-exit)))))
    (t
     (if (/= (length args) 1) (print-usage-and-exit))
     (values (car args) proxy-host proxy-port)))))

(defun standalone ()
  (multiple-value-bind (directory proxy-host proxy-port)
      (parse-args (get-argv))
    (handler-case
        (let ((drakma:*default-http-proxy*
               (cond
                 ((and proxy-host proxy-port) (list proxy-host proxy-port))
                 (proxy-host proxy-host))))
          (loop
             for thread-uri = (read-line nil nil)
             while thread-uri do
               (format t "Downloading thread ~a~%" thread-uri)
               (download-images thread-uri directory)
             finally (sb-ext:exit)))
      ((or #+sbcl
        sb-sys:interactive-interrupt
        file-error) ()
       (format *error-output* "Fatal error, quiting~%")
       (exit :code 1))))
  (exit))

(sb-ext:save-lisp-and-die cl-user::*executable-name*
                          :executable t
                          :toplevel #'standalone)
