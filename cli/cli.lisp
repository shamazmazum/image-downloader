(in-package :image-downloader-cli)

(defun ignored-types (string)
  (split-sequence:split-sequence
   #\, string))

(defun checksum-error-handler (string)
  (cond
    ((string= string "skip-file")
     :skip-file)
    ((string= string "ignore-error")
     :ignore-error)
    ((string= string "interactive")
     :interactive)
    (t (error 'error))))

(opts:define-opts
  (:name        :checksum-error-handler
   :description "What to do on checksum error? Can be skip-file, ignore-error or interactive."
   :short       #\c
   :long        "checksum-error"
   :arg-parser  #'checksum-error-handler)
  (:name        :ignore-types
   :description "Image types to ignore, a comma-separated list"
   :long        "ignore-types"
   :meta-var    "LIST"
   :arg-parser  #'ignored-types)
  (:name        :2ch-userauth-code
   :description "2ch.hk userauth cookie for accessing hidden boards"
   :long        "2ch-userauth-code"
   :meta-var    "CODE"
   :arg-parser  #'identity))

(defun print-usage-and-quit ()
  (opts:describe :usage-of "image-downloader"
                 :args "DIRECTORY")
  (uiop:quit 1))

(defun do-all-stuff (options arguments)
  (when (/= (length arguments) 1)
    (print-usage-and-quit))
  (let ((*ignore-types*              (getf options :ignore-types))
        (*checksum-error-response*   (getf options :checksum-error-handler :ignore-error))
        (directory (first arguments)))
    (let ((2ch-userauth-code (getf options :2ch-userauth-code)))
      (when 2ch-userauth-code
        (set-2ch-userauth-code 2ch-userauth-code)))
    (loop
      for thread-uri = (read-line *standard-input* nil)
      while thread-uri do
        (format t "Downloading thread ~a~%" thread-uri)
        (download-images thread-uri directory))))

(defun main ()
  (handler-case
      (multiple-value-bind (options arguments)
          (opts:get-opts)
        (do-all-stuff options arguments)
        (uiop:quit 0))
    (opts:troublesome-option ()
      (print-usage-and-quit))
    #+sbcl
    (sb-sys:interactive-interrupt ()
      (format *error-output* "Interrupted~%")
      (uiop:quit 1))))
