(in-package :image-downloader)

(defclass fishki-thread (imageboard-thread) ())

(defun fishki-image-p (list)
  ;; there is no constant tag enclosure around desired pictures.
  ;; try to guess on picture link (needed links contain "picsw")
  (let ((car (car list)))
    (and (listp car)
         (or (eq :a (car car))
             (eq :img (car car)))
         (some (lambda (obj)
                 (and (stringp obj)
                      (search "picsw" obj)))
             (cdr car)))))

(defun get-file-source/fishki (list)
  (let ((a/img-tag (car list)))
    (if (eq :img (car a/img-tag))
        (getf (cdr a/img-tag) :src)
        (getf (cdr a/img-tag) :href))))

(defmethod image-sources ((thread fishki-thread))
  (let ((files (search-in-tree (thread-body thread) #'fishki-image-p)))
    (mapcar (lambda (file)
              (let ((source (puri:merge-uris
                             (puri:parse-uri (get-file-source/fishki file))
                             (puri:parse-uri "http://fishki.net/"))))
                (cons source (after-last-slash (puri:uri-path source)))))
            files)))
