(in-package :image-downloader-cli)

(defun get-ignored-types (string)
  (split-sequence:split-sequence
   #\, string))

(opts:define-opts
  (:name        :ignore-checksums
   :description "Ignore checksum errors"
   :long        "ignore-checksum-errors")
  (:name        :ignore-types
   :description "Image types to ignore, a comma-separated list"
   :long        "ignore-types"
   :meta-var    "LIST"
   :arg-parser  #'get-ignored-types)
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
  (let ((*ignore-types*           (getf options :ignore-types))
        (*ignore-checksum-errors* (getf options :ignore-checksums))
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
