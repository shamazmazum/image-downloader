(defsystem :image-downloader
  :description "Imageboards image downloader"
  :serial t
  :version "1.0"
  :components ((:file "package")
               (:file "downloader")
               (:file "4chan")
               (:file "2ch")
               (:file "102chan")
               (:file "fishki"))
  :depends-on (:puri
               :drakma
               :cl-html-parse
               :split-sequence))
