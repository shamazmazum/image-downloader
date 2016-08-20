(in-package :image-downloader)

(defclass 2ch-thread (imageboard-thread) ())

(defun 2ch-image-p (list)
  (let ((car (car list)))
    (and (listp car)
         (eq :figure (car car))
         (string= "image " (getf (cdr car) :class)))))

(defun get-file-source/2ch (list)
  (getf
   (cdar
    (get-parameterized-tag
     (cdr (get-parameterized-tag (cdr list) :figcaption '(:class . "file-attr")))
     :a))
   :href))

(defmethod image-sources ((thread 2ch-thread))
  (let ((files (search-in-tree (thread-body thread) #'2ch-image-p)))
    (mapcar (lambda (file)
              (let ((source (puri:merge-uris
                             (puri:parse-uri (get-file-source/2ch file))
                             (thread-uri thread))))
                (cons source (after-last-slash (puri:uri-path source)))))
            files)))
