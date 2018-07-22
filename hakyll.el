;;; journal.el --- Convenience functions for working with journal.
;;
;;; Commentary:      
;; Convenience functions for working with journal.

;;; Code:

(defvar journal-site-location "~/journal/"
  "The location of the journal files.")

(defun journal-insert-image (alt-text)
  "Insert an image from /images/yyyy/mm to current buffer /posts/yyyy/mm/dd/....md"
  (interactive "sAltText: ")
  (let ((yyyymm (substring buffer-file-name 35 42)))
    (insert
     (format "![%s](/images/%s/thumbs/"
	     alt-text
	     yyyymm
	     ))
    )
  )

(fset 'ruby-tag-create-region
   "\C-w<ruby><rb>\C-y</rb><rt></rt></ruby>\C-b\C-b\C-b\C-b\C-b\C-b\C-b\C-b\C-b\C-b\C-b\C-b")

(fset 'hugo-youtube
   [?\C-e escape ?\C-r ?/ ?\\ ?| ?= return right ?\C-k ?\C-a ?\{ ?\{ ?< ?  ?y ?o ?u ?t ?u ?b ?e ?  ?\C-y ?  ?> ?\} ?\} ?\C-k right])


(defun journal-new-dream-post (title tags yyyy mm dd)
  "Create a new journal post for today with TITLE and TAGS."
  (interactive (list
		(read-string (format "Title: (%s):" "Dream") nil nil "Dream") 
		            (journal-read-tags nil)
                (read-string (format "Year (%s): " (format-time-string "%Y")) nil nil (format-time-string "%Y"))
                (read-string (format "Month (%s): " (format-time-string "%m")) nil nil (format-time-string "%m"))
                (read-string (format "Date (%s): " (format-time-string "%d")) nil nil (format-time-string "%d"))
		)
	       )
  (let (
	(file-name (journal-post-title dd title))
	(file-path (journal-post-path title yyyy mm dd))
	)
    (set-buffer (get-buffer-create file-path))
    (insert
     (format "---\ntitle: \"%s\"\ntags: [ %s ]\nauthor: Rob Nugen\ndate: %s-%s-%sT%s\n---\n\n%s\n\n"
             title
	     (mapconcat (lambda (x) (format "\"%s\"" (downcase x)))
                   tags ", ")
	     yyyy
	     mm
	     dd
	     (format-time-string "%H:%M:%S+09:00")
	     (format-time-string "## %H:%M %A %d %B %Y %Z")
	     ))
    (write-file
     (expand-file-name file-path (concat journal-site-location "")))
    (switch-to-buffer file-name)
    (auto-fill-mode)
  )
)

(defun journal-new-japanese-post (title tags yyyy mm dd)
  "Create a new journal post for today with TITLE and TAGS."
  (interactive (list
		(read-string (format "Title: (%s):" "日本語を勉強しました") nil nil "日本語を勉強しました")
                (journal-read-tags nil)
                (read-string (format "Year (%s): " (format-time-string "%Y")) nil nil (format-time-string "%Y"))
                (read-string (format "Month (%s): " (format-time-string "%m")) nil nil (format-time-string "%m"))
                (read-string (format "Date (%s): " (format-time-string "%d")) nil nil (format-time-string "%d"))
		)
	       )
  (let (
	(file-name (journal-post-title dd "studied japanese"))
	(file-path (journal-post-path "studied japanese" yyyy mm dd))
	)
    (set-buffer (get-buffer-create file-path))
    (insert
     (format "---\ntitle: \"%s\"\ntags: [ %s ]\nauthor: Rob Nugen\ndate: %s-%s-%sT%s\n---\n\n%s\n\n### There are probably errors in the Japanese notes below!\n\n"
             title
	     (mapconcat (lambda (x) (format "\"%s\"" (downcase x)))
                   tags ", ")
	     yyyy
	     mm
	     dd
	     (format-time-string "%H:%M:%S+09:00")
	     (format-time-string "## %H:%M %A %d %B %Y %Z")
	     ))
    (write-file
     (expand-file-name file-path (concat journal-site-location "")))
    (switch-to-buffer file-name)
    (auto-fill-mode)
  )
)

(defun journal-new-post (title tags yyyy mm dd)
  "Create a new journal post for today with TITLE and TAG."
  (interactive (list
                (read-string "Title: ")
                (journal-read-tags nil)
                (read-string (format "Year (%s): " (format-time-string "%Y")) nil nil (format-time-string "%Y"))
                (read-string (format "Month (%s): " (format-time-string "%m")) nil nil (format-time-string "%m"))
                (read-string (format "Date (%s): " (format-time-string "%d")) nil nil (format-time-string "%d"))
                )
               )
  (let (
        (file-name (journal-post-title dd title))
        (file-path (journal-post-path title yyyy mm dd))
        )
    (set-buffer (get-buffer-create file-path))
    (insert
     (format "---\ntitle: \"%s\"\ntags: [ %s ]\nauthor: Rob Nugen\ndate: %s-%s-%sT%s\n---\n\n%s\n\n"
             title
             (mapconcat (lambda (x) (format "\"%s\"" (downcase x)))
                   tags ", ")
             yyyy
             mm
             dd
             (format-time-string "%H:%M:%S+09:00")
             (format-time-string "## %H:%M %A %d %B %Y %Z")
             ))
    (write-file
     (expand-file-name file-path (concat journal-site-location "")))
    (switch-to-buffer file-name)
    (auto-fill-mode)
  )
)

(defun my-test (title tags)
   (interactive (list (read-string "Title: ") (journal-read-tags)))
   (message "%s: %s" title

(mapconcat (lambda (x) (format "'%s'" x))
           tags ", ")

            ))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman 〔zzbba…@aol.com〕”. 2010-09-02
;; via http://ergoemacs.org/emacs/elisp_read_file_content.html  2018-07-21

(defun journal-read-tags (tags)
  (let (tag done)
    (while (not done)
      (setq tag (read-string "Tag: "))
      (if (= (length tag) 0)
          (setq done t)
        (push tag tags)))
    (nreverse tags)))

;;;  http://stackoverflow.com/a/251922


(defvar current-date-time-format "## %H:%M %A %d %B %Y %Z"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "## %H:%M"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")


(defun markdown-timestamp-full ()
  "Insert the current date and time into current buffer.  Uses `current-date-time-format' for the formatting the date/time."
       (interactive)
       (insert (format-time-string current-date-time-format (current-time)))
       (insert "\n")
       )

(defun markdown-timestamp-short ()
  "Insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
  )

(defun journal-new-note (title)
  "Create a new Note with TITLE."
  (interactive "sTitle: ")
  (let ((file-name (journal-note-title title)))
    (set-buffer (get-buffer-create file-name))
    (insert (format "---\ntitle: %s\ndescription: \n---\n\n" title))
    (write-file
     (expand-file-name file-name (concat journal-site-location "notes")))
    (switch-to-buffer file-path)))

(defun journal-post-title (dd title)
  "Return a file name based on TITLE for the post."
  (concat dd
   (url-safe-string title)
   ".md"))

(defun journal-post-path (title yyyy mm dd)
  "Return a file path based on TITLE and date."
  (concat
   yyyy "/" mm "/" dd
   (url-safe-string title)
   ".md"))

(defun journal-note-title (title)
  "Return a file name based on TITLE for the note."
  (concat
   (url-safe-string title)
   ".md"))

(defun url-safe-string (title)
  "Return a URL-safe title based on TITLE."
  (replace-regexp-in-string "[:!']" ""
    (replace-regexp-in-string " " "-" (downcase title))
    )
  )


(provide 'journal)
;;; hakyll.el ends here
