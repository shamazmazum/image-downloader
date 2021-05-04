(in-package :image-downloader)

(defclass fishki-thread (html-resource) ())

(defun get-parameterized-tag (list tag &rest parameters)
  "Find a tag with parameters and its body in parsed HTML"
  (find-if (lambda (obj)
             (and (listp obj)
                  (listp (car obj))
                  (eq (caar obj) tag)
                  (every (lambda (parameter)
                           (string= (cdr parameter)
                                    (getf (cdar obj) (car parameter))))
                         parameters)))
           list))

(defun search-in-tree (tree predicate)
  "Recursively search in parsed HTML tree for something"
  (labels ((perform-search (acc tree)
             (if (listp tree)
                 (cond
                   ((funcall predicate tree) (cons tree acc))
                   ((cdr tree) (reduce #'perform-search (cdr tree)
                                       :initial-value acc))
                   (t acc))
                 acc)))
    (perform-search nil tree)))

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

(defun fishki-image/2-p (list)
  ;; Also try find <div class="picture-relative">
  (let ((car (car list)))
    (equalp '(:div :class "picture-relative") car)))

(defun get-file-source/fishki (list)
  (let ((a/img-tag (car list)))
    (if (eq :img (car a/img-tag))
        (getf (cdr a/img-tag) :src)
        (getf (cdr a/img-tag) :href))))

(defun get-file-source/fishki/2 (list)
  (getf
   (cdar
    (get-parameterized-tag
     (cdr
      (get-parameterized-tag (cdr list) :a))
     :img))
   :src))

(defmethod image-sources ((thread fishki-thread))
  (let ((files1 (search-in-tree (resource-body thread) #'fishki-image-p))
        (files2 (search-in-tree (resource-body thread) #'fishki-image/2-p)))
    (union
     (mapcar (lambda (file)
               (let ((source (puri:merge-uris
                              (puri:parse-uri (get-file-source/fishki file))
                              (puri:parse-uri "http://fishki.net/"))))
                 (make-instance 'image :uri source :name (after-last-slash (puri:uri-path source)))))
             files1)
     (mapcar (lambda (file)
               (let ((source (get-file-source/fishki/2 file)))
                 (make-instance 'image
                                :uri (puri:merge-uris
                                      (puri:parse-uri source)
                                      (puri:parse-uri "https:"))
                                :name (after-last-slash source))))
             files2))))
