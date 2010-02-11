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

(defvar *ascii-characters*  (loop for i from 32 to 126 collect (code-char i)))

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

(defun filter-printable-chars (string)
  (remove-if-not #'(lambda (ch) (member ch (coerce *ascii-characters* 'list))) string))

(defun synchsafe (bytes)
  (reduce #'(lambda (x a)
	      (logior (ash x 7) a)) bytes :initial-value 0))

(defun read-n-from (stream n &optional (start 0))
  (if (not (open-stream-p stream))
      nil
      (let ((arr (make-array n)))
	(file-position stream start)
	(read-sequence arr stream)
	arr)))

(defun id3-version (filename)
  (with-open-file (file filename)
    (let* ((len (file-length file))
	   (tagpos (- len 128)))
      (cond ((< len 128) 'unknown)
	    ((string= (coerce (read-n-from file 3) 'string) "ID3") 'id3v2)
	    (t
	     (if (string= (coerce (read-n-from file 3 tagpos) 'string) "TAG")
		 'id3v1
		 'unknown))))))

(defun file-size (filename)
  (with-open-file (f filename) (file-length f)))

(defmacro read-and-filter (seq stream)
  `(progn
     (read-sequence ,seq ,stream)
     (setf ,seq (filter-printable-chars ,seq))))

(defun read-id3-tags (filename)
  (let ((version (id3-version filename)))
    (case version
      ('id3v1 (read-id3v1-tags filename))
      ;('id3v2 (read-id3v2-tags filename))
      (otherwise nil))))

(defun read-id3v1-tags (filename)
  (let ((length (file-size filename)))
    (if (< length 128)
	nil
	(let ((pos (- length 125))
	      (song (make-song-info :title (make-string 30)
				    :artist (make-string 30)
				    :album (make-string 30)
				    :genre 0)))
	  (with-open-file (file filename)
	    (file-position file pos)
	    (read-and-filter (song-info-title song) file)
	    (read-and-filter (song-info-artist song) file)
	    (read-and-filter (song-info-album song) file)
	    (file-position file (- length 1))
	    (setf (song-info-genre song) (char-code (coerce (read file) 'character))))
	  song))))
