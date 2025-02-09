(in-package :image-downloader)

;;Classes

(defclass resource ()
  ((uri  :accessor      resource-uri
         :initarg       :uri
         :initform      (error "You must specify an URI")
         :documentation "URI of the resource")
   (body :accessor      resource-body
         :documentation "Downloaded body of the resource in some form"))
  (:documentation "Generic resource class"))

(defclass imageboard-thread (resource)
  ((board       :accessor imageboard-board
                :type string)
   (thread-id   :accessor imageboard-thread-id
                :type (or unsigned-byte string))
   (thread-name :accessor imageboard-thread-name
                :type string))
  (:documentation "Generic class for an imageboard class"))

(defclass json-api-resource (resource)
  ()
  (:documentation "Generic class for a resource that has a JSON API"))

(defclass image ()
  ((name :initarg  :name
         :initform (error "Specify image name")
         :reader   image-name)
   (uri  :initarg  :uri
         :initform (error "Specify image URI")
         :reader   image-uri)
   (data :initarg  :data
         :initform #()
         :type     simple-vector
         :accessor image-data))
  (:documentation "Class representing an image from a resource"))

(defclass image-md5 (image)
  ((md5 :initarg  :md5
        :initform (error "Specify MD5 checksum")
        :reader   image-md5))
  (:documentation "MD5 protected image"))

;; Generic functions

(defgeneric download-resource (resource)
  (:documentation "Dowload and parse a thread from the resource"))

(defgeneric directory-name (resource)
  (:documentation "Guess directory name for saved files from the name
of a thread"))

(defgeneric image-sources (resource)
  (:documentation "Get images sources and their names"))

(defgeneric check-image (image)
  (:documentation "Check downloaded image for correctness.
Signals a condition BAD-CHECKSUM in case of an error."))
