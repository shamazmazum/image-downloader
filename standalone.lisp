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
  (format *error-output* "echo thread-uri | image-downloader <directory>~%")
  (sb-ext:exit))

(defun parse-args (argv)
  (if (/= (length argv) 1) (print-usage-and-exit))
  (car argv))

(defun standalone ()
  (let ((directory (parse-args (get-argv))))
    (handler-case
        (loop
           for thread-uri = (read-line nil nil)
           while thread-uri do
             (format t "Downloading thread ~a~%" thread-uri)
             (download-images thread-uri directory)
           finally (sb-ext:exit))
      ((or #+sbcl
        sb-sys:interactive-interrupt
        file-error) ()
       (format *error-output* "Fatal error, quiting~%")
       (exit :code 1))))
  (exit))

(sb-ext:save-lisp-and-die cl-user::*executable-name*
                          :executable t
                          :toplevel #'standalone)
