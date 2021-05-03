(in-package :image-downloader)

(define-condition image-downloader-error (error)
  ((uri  :initarg :uri
         :reader  bad-uri))
  (:documentation "Generic image-downloader error"))

(define-condition bad-response-code (image-downloader-error)
  ((code :initarg :code
         :reader  bad-code))
  (:documentation "Signaled when bad response (not 200 OK) is returned
from HTTP request")
  (:report (lambda (c s)
             (format s "Bad response code: ~d (URI: ~a)"
                     (bad-code c)
                     (bad-uri  c)))))

(define-condition unknown-resource (image-downloader-error)
  ()
  (:documentation "Signaled when trying to download images from an
unknown resource")
  (:report (lambda (c s)
             (format s "Unknown resource (URI: ~a)"
                     (bad-uri c)))))

(define-condition bad-checksum (image-downloader-error)
  ()
  (:documentation "Signalled when downloaded image has invalid
checksum")
  (:report (lambda (c s)
             (format s "Bad checksum (URI: ~a)"
                     (bad-uri c)))))
