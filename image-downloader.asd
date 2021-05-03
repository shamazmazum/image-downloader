(defsystem :image-downloader
  :description "Imageboards image downloader"
  :serial t
  :version "1.0"
  :components ((:file "package")
               (:file "conditions")
               (:file "classes")
               (:file "methods")
               (:file "downloader")
               (:file "4chan")
               (:file "2ch")
               (:file "fishki"))
  :depends-on (:puri
               :drakma
               :cl-html-parse
               :cl-json
               :split-sequence
               :md5
               :s-base64))
