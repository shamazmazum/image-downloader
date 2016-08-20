PROG_NAME = image-downloader

${PROG_NAME}: *.lisp
	sbcl --eval '(progn (defvar *executable-name* "${.TARGET}") (asdf:load-system :image-downloader) (load "standalone.lisp"))'

clean:
	rm -f ${PROG_NAME}

.PHONY: clean
