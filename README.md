This is a simple image downloader, which can download images from imageboards
(currently 4chan and 2ch). It depends on `drakma` http client and
`cl-html-parse` html parser.

You can use it loading the ASDF system and calling
image-downloader:download-images:

    (asdf:load-system :image-downloader)
    (image-downloader:download-images uri-string path-to-saved-images)
