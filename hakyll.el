;;; hakyll.el --- Convenience functions for working with Hakyll.
;;
;;; Commentary:      
;; Convenience functions for working with Hakyll.

;;; Code:

(defvar hakyll-site-location "~/journal/"
  "The location of the Hakyll files.")

(defun hakyll-new-post (title tags yyyy mm dd)
  "Create a new Hakyll post for today with TITLE and TAGS."
  (interactive "sTitle: \nsTags: \nsYear: \nsMonth: \nsDay: ")
  (let ((file-name (hakyll-post-title title))
	(file-path (hakyll-post-path title yyyy mm dd)))
    (set-buffer (get-buffer-create file-path))
    (insert
     (format "---\ntitle: %s\ntags: %s\ndescription: \nauthor: Rob Nugen\ndate: %s-%s-%s\n---\n\n%s\n"
             title
	     (downcase tags)
	     yyyy
	     mm
	     dd
	     (format-time-string "##%H:%M %A %d %B %Y %Z##")
        ))
    (write-file
     (expand-file-name file-path (concat hakyll-site-location "posts")))
    (switch-to-buffer file-name)
    ))

(defun hakyll-new-note (title)
  "Create a new Note with TITLE."
  (interactive "sTitle: ")
  (let ((file-name (hakyll-note-title title)))
    (set-buffer (get-buffer-create file-name))
    (insert (format "---\ntitle: %s\ndescription: \n---\n\n" title))
    (write-file
     (expand-file-name file-name (concat hakyll-site-location "notes")))
    (switch-to-buffer file-name)))

(defun hakyll-post-title (title)
  "Return a file name based on TITLE for the post."
  (concat
   (replace-regexp-in-string " " "-" (downcase title))
   ".markdown"))

(defun hakyll-post-path (title yyyy mm dd)
  "Return a file path based on TITLE and date."
  (concat
   yyyy "/" mm "/" dd "/"
   (replace-regexp-in-string " " "-" (downcase title))
   ".markdown"))

(defun hakyll-note-title (title)
  "Return a file name based on TITLE for the note."
  (concat
   (replace-regexp-in-string " " "-" (downcase title))
   ".markdown"))

(provide 'hakyll)
;;; hakyll.el ends here
