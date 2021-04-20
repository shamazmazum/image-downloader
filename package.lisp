(defpackage image-downloader
  (:use #:cl #:split-sequence)
  (:export #:download-images
           #:set-2ch-userauth-code
           #:*ignored-extensions*
           #:*ignore-checksum-errors*))
