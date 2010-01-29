(in-package :cl-user)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (asdf:oos 'asdf:load-op :cl-fad))

(defpackage :cl-mediaserver
  (:use :cl-fad
	:cl)
  (:export :search-media))

(in-package :cl-mediaserver)

;; the media server code

(defparameter *songs* nil)

(defun search-media (directory)
  (declare (ftype (function (string) (vector t)) search-media))
  (let ((songs (make-array 100 :element-type 'pathname :adjustable t :fill-pointer 0)))
    (cl-fad:walk-directory
     directory
     #'(lambda (path)
	 (when (equal "mp3" (pathname-type path))
	   (vector-push-extend path songs))))
    songs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ID3 parsing code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all we really need out of the ID3
;; tag is the title, artist, album, genre

(defstruct song-info
  title
  artist
  album
  genre)

(defun synchsafe (bytes)
  (reduce #'(lambda (x a)
	      (logior (ash x 7) a)) bytes :initial-value 0))

(defun id3-version (filename)
  (with-open-file (file filename)
    (let* ((len (file-length file))
	   (tagpos (- len 128))
	   (tag (make-array 3)))
      (if (<= tagpos 0)
	  'unknown
	  (progn
	    (file-position file tagpos)
	    (read-sequence tag file)
	    (cond ((string= (coerce tag 'string) "TAG") 'id3v1)
		  (t (progn
		       (file-position file 0)
		       (read-sequence tag file)
		       (if (string= (coerce tag 'string) "ID3")
			   'id3v2
			   'unknown)))))))))

