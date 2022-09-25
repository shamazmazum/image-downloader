(defpackage image-downloader
  (:use #:cl #:split-sequence #:destructuring-bind-plus)
  (:local-nicknames (:alex :alexandria))
  (:export #:download-images
           #:set-2ch-userauth-code
           #:*ignore-types*
           #:*ignore-checksum-errors*))
