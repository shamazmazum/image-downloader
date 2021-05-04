(defpackage image-downloader
  (:use #:cl #:split-sequence #:destructuring-bind-plus)
  (:export #:download-images
           #:set-2ch-userauth-code
           #:*ignore-types*
           #:*ignore-checksum-errors*))
