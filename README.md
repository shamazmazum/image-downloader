This is a simple image downloader, which can download images from imageboards
(currently 4chan and 2ch). It depends on `drakma` http client and
`cl-html-parse` html parser.

You can use it loading the ASDF system and calling
image-downloader:download-images:

    (asdf:load-system :image-downloader)
    (image-downloader:download-images uri-string path-to-saved-images)

Also you can make a standalone application just running `make`. It will download
all threads supplied on standard input, one thread URI per line. An example:

    echo "http://4chan.org/b/bla-bla" | image-downloader path-to-saved-images
