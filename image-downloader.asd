(defsystem :image-downloader
  :description "Imageboards image downloader"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :version "1.2"
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "classes")
               (:file "methods")
               (:file "downloader")
               (:file "4chan")
               (:file "2ch"))
  :depends-on (:alexandria
               :puri
               :dexador
               :cl-json
               :split-sequence
               :md5
               :s-base64
               :destructuring-bind-plus))

(defsystem :image-downloader/executable
  :name :image-downloader/executable
  :version "1.2"
  :pathname "cli"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :serial t
  :components ((:file "package")
               (:file "cli"))
  :depends-on (:image-downloader
               :split-sequence
               :command-line-parse)
  :build-operation program-op
  :build-pathname "image-downloader"
  :entry-point "image-downloader/cli:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))
