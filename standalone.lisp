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
  (format *error-output* "echo thread-uri | image-downloader [--proxy-host proxy-host] [--ignore-extension extension1 [--ignore-extension extension2 [...]]] [--proxy-port proxy-port] <directory>~%")
  #+nil (sb-ext:exit))

(defun parse-args (args)
  (let (proxy-host proxy-port ignored-extensions path)
    (loop for arg = (pop args) while arg do
         (cond
           ((string= "--proxy-host" arg)
            (setq proxy-host (pop args)))
           ((string= "--proxy-port" arg)
            (setq proxy-port
                  (handler-case
                      (parse-integer (pop args))
                    (parse-error () (print-usage-and-exit)))))
           ((string= "--ignore-extension" arg)
            (push (pop args) ignored-extensions))
           (t (if args (print-usage-and-exit))
              (setq path arg)))
       finally
         (if (not path) (print-usage-and-exit))
         (return
           (values path proxy-host proxy-port ignored-extensions)))))

(defun standalone ()
  (multiple-value-bind (directory proxy-host proxy-port ignored-extensions)
      (parse-args (get-argv))
    (handler-case
        (let ((drakma:*default-http-proxy*
               (cond
                 ((and proxy-host proxy-port) (list proxy-host proxy-port))
                 (proxy-host proxy-host)))
              (*ignored-extensions* ignored-extensions))
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
