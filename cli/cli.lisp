(in-package :image-downloader/cli)

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
    (t (error 'clp:cmd-line-parse-error
              :format-control "Invalid checksum error handler"))))

(defparameter *options-parser*
  (clp:seq
   (clp:optional
    (clp:option :checksum-error-handler
                "HANDLER"
                :short       #\c
                :long        "checksum-error"
                :description "What to do on checksum error? Can be skip-file, ignore-error or interactive."
                :fn #'checksum-error-handler)
    (clp:option :ignore-types
                "LIST"
                :long        "ignore-types"
                :description "Image types to ignore, a comma-separated list"
                :fn          #'ignored-types)
    (clp:option :2ch-userauth-code
                "COOKIE"
                :long        "2ch-userauth-code"
                :description "2ch.hk userauth cookie for accessing hidden boards"))
   (clp:argument :directory "DIRECTORY")))

(defun %do-all-stuff (options)
  (let ((*ignore-types*              (clp:%assoc :ignore-types options))
        (*checksum-error-response*   (clp:%assoc :checksum-error-handler options
                                                 :ignore-error))
        (directory                   (clp:%assoc :directory options))
        (2ch-userauth-code           (clp:%assoc :2ch-userauth-code options)))
    (when 2ch-userauth-code
      (set-2ch-userauth-code 2ch-userauth-code))
    (loop
      for thread-uri = (read-line *standard-input* nil)
      while thread-uri do
        (format t "Downloading thread ~a~%" thread-uri)
        (download-images thread-uri directory))))

(defun main ()
  (handler-case
      (progn
        (%do-all-stuff (clp:parse-argv *options-parser*))
        (uiop:quit 0))
    (clp:cmd-line-parse-error ()
      (clp:print-usage *options-parser* "image-downloader")
      (uiop:quit 1))
    #+sbcl
    (sb-sys:interactive-interrupt ()
      (format *error-output* "Interrupted~%")
      (uiop:quit 1))))
