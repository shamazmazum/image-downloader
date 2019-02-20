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
  (format *error-output* "echo thread-uri | image-downloader [--http-proxy proxy-url] [--ignore-extension extension1 [--ignore-extension extension2 [...]]] <directory>~%")
  #+sbcl (sb-ext:exit))

(defun parse-args (args)
  (let (http-proxy ignored-extensions path)
    (loop for arg = (pop args) while arg do
         (cond
           ((string= "--http-proxy" arg)
            (setq http-proxy (pop args)))
           ((string= "--ignore-extension" arg)
            (push (pop args) ignored-extensions))
           ((string= "--2ch-userauth-code" arg)
            (set-2ch-userauth-code (pop args)))
           (t (if args (print-usage-and-exit))
              (setq path arg)))
       finally
         (if (not path) (print-usage-and-exit))
         (return
           (values path http-proxy ignored-extensions)))))

(defun proxy-host-and-port (http-proxy)
  (let ((uri (puri:parse-uri http-proxy)))
    (values
     (puri:uri-host uri)
     (puri:uri-port uri))))

(defun get-proxy-env ()
  #+sbcl
  (progn
    (let* ((env (sb-ext:posix-environ))
           (parsed-env (mapcar (lambda (env) (split-sequence:split-sequence #\= env)) env))
           (proxy-env (or (find "http_proxy" parsed-env :test #'string= :key #'first)
                          (find "HTTP_PROXY" parsed-env :test #'string= :key #'first))))
      (proxy-host-and-port (second proxy-env))))
  #-sbcl nil)

(defun standalone ()
  (multiple-value-bind (directory http-proxy ignored-extensions)
      (parse-args (get-argv))
    (handler-case
        (let ((drakma:*default-http-proxy*
               (cond
                 ((proxy-host-and-port http-proxy)
                  (multiple-value-list (proxy-host-and-port http-proxy)))
                 ((get-proxy-env)
                  (multiple-value-list (get-proxy-env)))))
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
