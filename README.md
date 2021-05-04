This is a simple image downloader, which can download images from imageboards
(currently 4chan and 2ch).

You can use it loading the ASDF system and calling
image-downloader:download-images:

    (ql:quickload :image-downloader)
    (image-downloader:download-images uri-string path-to-saved-images)

Also you can make a standalone application by running

    (ql:quickload :image-downloader/executable)
    (asdf:make :image-downloader/executable)

It will download all threads supplied on the standard input, one thread URI per
line. An example:

    echo "http://4chan.org/b/bla-bla" | image-downloader path-to-saved-images

Look at the full list of options by running `image-downloader` without
arguments.
