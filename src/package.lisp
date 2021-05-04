(defpackage image-downloader
  (:use #:cl #:split-sequence)
  (:export #:download-images
           #:set-2ch-userauth-code
           #:*ignore-types*
           #:*ignore-checksum-errors*))
