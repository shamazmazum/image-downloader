(defpackage image-downloader/cli
  (:use #:cl #:image-downloader)
  (:local-nicknames (#:clp #:command-line-parse))
  (:export #:main))
