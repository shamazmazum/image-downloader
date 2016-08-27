;; Standalone downloader and building script.
;; To get a standalone application, run make instead of doing something with this file

(defpackage image-downloader-standalone
  (:use #:cl #:image-downloader))

(defvar *executable-name*) ; to silence warnings

(in-package :image-downloader-standalone)

(defun get-argv ()
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl (error "Do not know how to get argv, ha-ha!"))

(defun print-usage-and-exit ()
  (format *error-output* "echo thread-uri | image-downloader [--interactive] <directory>~%")
  (sb-ext:exit))

(defun parse-args (argv)
  (if (< (length argv) 1) (print-usage-and-exit))
  (when (string= (first argv) "--interactive")
    (if (/= (length argv) 2)
        (print-usage-and-exit))
    (return-from parse-args (values (second argv) t)))
  (if (/= (length argv) 1) (print-usage-and-exit))
  (values (first argv) nil))

(defun standalone ()
  (multiple-value-bind (directory interactive)
      (parse-args (get-argv))
    (let ((*interactive* interactive))
      (with-simple-restart (toplevel-skip "Skip downloading threads")
        (loop
           for thread-uri = (read-line nil nil)
           while thread-uri do
             (format t "Downloading thread ~a~%" thread-uri)
             (download-images thread-uri directory)
           finally (sb-ext:exit))))))

(sb-ext:save-lisp-and-die cl-user::*executable-name*
                          :executable t
                          :toplevel #'standalone)
