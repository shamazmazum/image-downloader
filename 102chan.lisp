(in-package :image-downloader)

(defclass 102chan-thread (imageboard-thread) ())

;<span class="filesize">Файл: <a href="../src/1471460955062.jpg">1471460955062.jpg</a>&ndash;(160.46KB, 650x871)</span>

(defun 102chan-image-p (list)
  (let ((car (car list)))
    (and (listp car)
         (eq :span (car car))
         (string= "filesize" (getf (cdr car) :class)))))

(defun get-file-source/102chan (list)
  (getf
   (cdar
    (get-parameterized-tag (cdr list) :a))
   :href))

(defmethod image-sources ((thread 102chan-thread))
  (let ((files (search-in-tree (thread-body thread) #'102chan-image-p)))
    (mapcar (lambda (file)
              (let ((source (puri:merge-uris
                             (puri:parse-uri (get-file-source/102chan file))
                             (thread-uri thread))))
                (cons source (after-last-slash (puri:uri-path source)))))
            files)))
