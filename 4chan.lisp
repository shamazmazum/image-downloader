(in-package :image-downloader)

(defclass 4chan-thread (imageboard-thread) ())

(defun div-class-file-p (list)
  (let ((car (car list)))
    (and (listp car)
         (eq :div (car car))
         (string= "file" (getf (cdr car) :class)))))

(defun get-file-source/4chan (list)
  (getf
   (cdar
    (get-parameterized-tag
     (cdr (get-parameterized-tag (cdr list) :div '(:class . "fileText")))
     :a))
   :href))

(defmethod image-sources ((thread 4chan-thread))
  (let ((files (search-in-tree (thread-body thread) #'div-class-file-p)))
    (reduce (lambda (acc file)
              (let ((source (get-file-source/4chan file)))
                (if source
                    (cons (cons (concatenate 'string "http:" source)
                                (after-last-slash source)) acc)
                    acc)))
            files
            :initial-value nil)))
