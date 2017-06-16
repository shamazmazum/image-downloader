(defpackage image-downloader
  (:use #:cl #:split-sequence)
  (:export #:download-images
           #:*ignored-extensions*))
