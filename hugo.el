;;;    hugo.el --- functions for adding entries in a Hugo site.
;;;    Copyright (C) 2013 Rob Nugen
;;;
;;;    This program is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;

(defvar journal-site-location "~/journal/"
  "The location of the journal files (in their own repo).")

(defvar blog-site-location "~/barefoot_rob/content/blog/"
  "The location of the blog files (included in new.robnugen.com site itself).")

(defvar event-entry-location "~/barefoot_rob/content/events/"
  "Event entries might eventually be driven by an ics file, but that will probably never happen so let's see how long I can maintain them this way.")

(defvar mt3-site-location "~/mt3.com/content/"
  "The location of Marble Track 3 site.")

(defvar location-journal-template-files "~/.emacs.d/personal/hugo/templates/"
  "The location of templates used in this file")

(defvar location-atom-snippets "~/.atom/snippets.cson"  "Atom uses these for autocompletes")

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
     (format
       (get-string-from-file (expand-file-name "dream_template.txt" location-journal-template-files))
       title
	     (mapconcat (lambda (x) (format "\"%s\"" (downcase x)))  tags ", ")
	     yyyy
	     mm
	     dd
	     (format-time-string "%H:%M:%S+09:00")
	     (format-time-string "%H:%M %A %d %B %Y %Z")
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
     (format
       (get-string-from-file (expand-file-name "nihongo_template.txt" location-journal-template-files))
       title
	     (mapconcat (lambda (x) (format "\"%s\"" (downcase x)))  tags ", ")
	     yyyy
	     mm
	     dd
	     (format-time-string "%H:%M:%S+09:00")
	     (format-time-string "%H:%M %A %d %B %Y %Z")
	   )
    )
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
     (format
             (get-string-from-file (expand-file-name "journal_template.txt" location-journal-template-files))
             title
             (mapconcat (lambda (x) (format "\"%s\"" (downcase x))) tags ", ")
             yyyy
             mm
             dd
             (format-time-string "%H:%M:%S+09:00")
             (format-time-string "%H:%M %A %d %B %Y %Z")
             ))
    (write-file
     (expand-file-name file-path (concat journal-site-location "")))
    (switch-to-buffer file-name)
    (auto-fill-mode)
  )
)

;;;;;;;;;;;;;;   begin event post   PUBLISH on yyyy/mm/dd   and post in the event_yyyy/event_mm/ directory

(defun event-new-post (event_yyyy event_mm event_dd event_time event_location title tags yyyy mm dd)
  "Create a new event with its own event date and publish date with TITLE, and TAGs."
  (interactive (list
                (read-string (format "Event Year (%s): " (format-time-string "%Y")) nil nil (format-time-string "%Y"))
                (read-string (format "Event Month (%s): " (format-time-string "%m")) nil nil (format-time-string "%m"))
                (read-string (format "Event Date (%s): " (format-time-string "%d")) nil nil (format-time-string "%d"))
                (read-string "Time: ")
                (read-string "Location: ")
                (read-string "Title: ")
                (journal-read-tags nil)
                (read-string (format "Year (%s): " (format-time-string "%Y")) nil nil (format-time-string "%Y"))
                (read-string (format "Month (%s): " (format-time-string "%m")) nil nil (format-time-string "%m"))
                (read-string (format "Date (%s): " (format-time-string "%d")) nil nil (format-time-string "%d"))
                )
               )
  (let (
        (file-name (journal-post-title event_dd title))
        (file-path (journal-post-path title event_yyyy event_mm event_dd))
        )
    (set-buffer (get-buffer-create file-path))
    (insert
     (format
             (get-string-from-file (expand-file-name "event_template.txt" location-journal-template-files))
             title
             (mapconcat (lambda (x) (format "\"%s\"" (downcase x))) tags ", ")
             yyyy
             mm
             dd
             (format-time-string "%H:%M:%S+09:00")
	     (format "%s %s" event_time (format-time-string "%A %e %B %Y" (date-to-time (format "%s-%s-%sT12:00:00+0900" event_yyyy event_mm event_dd))))
	     event_location
             ))
    (write-file
     (expand-file-name file-path (concat event-entry-location "")))
    (switch-to-buffer file-name)
    (auto-fill-mode)
  )
)

;;;;;;;;;;;;;;   end event post
;;;;;;;;;;;;;;   begin blog post

(defun blog-new-post (title tags yyyy mm dd)
  "Create a new blog post for today with TITLE and TAG."
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
     (format
             (get-string-from-file (expand-file-name "blog_template.txt" location-journal-template-files))
             title
             (mapconcat (lambda (x) (format "\"%s\"" (downcase x))) tags ", ")
             yyyy
             mm
             dd
             (format-time-string "%H:%M:%S+09:00")
             ))
    (write-file
     (expand-file-name file-path (concat blog-site-location "")))
    (switch-to-buffer file-name)
    (auto-fill-mode)
  )
)

;;;;;;;;;;;;;;   end blog post

(defun new-storylog-post (title tags yyyy mm dd)
  "Create a new storylog for Step on Red Dot speech stories on DATE with TITLE and TAG."
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
     (format (get-string-from-file (expand-file-name "storylog_template.txt" location-journal-template-files))
             title
             (mapconcat (lambda (x) (format "\"%s\"" (downcase x)))  tags ", ")
             yyyy
             mm
             dd
             (format-time-string "%H:%M:%S+09:00")
             (format-time-string "%H:%M %A %d %B %Y %Z")
             ))
    (write-file
     (expand-file-name file-path (concat journal-site-location "")))
    (switch-to-buffer file-name)
    (auto-fill-mode)
  )
)

(defun mt3-new-episode (title tags image youtubeurl yyyy mm dd)
  "Create a new episode for Marble Track 3 .com with TITLE and TAGs."
  (interactive (list
                (read-string "Title: ")
                (journal-read-tags nil)
		            (read-string "Image: " nil nil "/img/guest/default-guest.png")
	            	(read-string "YouTube: " nil nil "https://www.youtube.com/watch?v=cbbXuNWLp6A")
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
     (format (get-string-from-file (expand-file-name "mt3_episode_template.txt" location-journal-template-files))
             title
             (mapconcat (lambda (x) (format "\"%s\"" (downcase x)))	tags ", ")
      	     (youtube-id youtubeurl)
	           image
	           (thumbnail-path image)
             yyyy
             mm
             dd
             (format-time-string "%H:%M:%S+09:00")
             (format-time-string "%H:%M %A %d %B %Y %Z")
             ))
    (write-file
     (expand-file-name file-path (concat mt3-site-location "episode")))
    (switch-to-buffer file-name)
    (auto-fill-mode)
  )
)

(defun mt3-new-part (title shortcode tags image yyyy mm dd)
  "Create a new track part for Marble Track 3 .com with TITLE, TAGs, and shortcode."
  (interactive
    (list
      (read-string "Title: ")
      (read-string "Shortcode: ")
      (journal-read-tags nil)
      (read-string "Image: " nil nil "/img/guest/default-guest.png")
      (read-string (format "Year (%s): " (format-time-string "%Y")) nil nil (format-time-string "%Y"))
      (read-string (format "Month (%s): " (format-time-string "%m")) nil nil (format-time-string "%m"))
      (read-string (format "Date (%s): " (format-time-string "%d")) nil nil (format-time-string "%d"))
    )
  )
  (let (
        (file-name (journal-post-title dd title))
        (file-path (journal-no-date-path title))
        )
    (switch-to-buffer (find-file-noselect location-atom-snippets))
    (end-of-buffer)
         (insert
          (format
          (get-string-from-file (expand-file-name "snippet_part.txt" location-journal-template-files))
           title
     	     shortcode
           title
     	     (file-name-sans-extension file-path)  ;; https://stackoverflow.com/a/8716106/194309
     	     ))
	 (save-buffer)
	 (kill-buffer)
    (set-buffer (get-buffer-create file-path))
    (insert
      (format
         (get-string-from-file (expand-file-name "mt3_parts_template.txt" location-journal-template-files))
         title
         (mapconcat (lambda (x) (format "\"%s\"" (downcase x)))	tags ", ")
         image
         (thumbnail-path image)
         title
         shortcode
         yyyy
         mm
         dd
         (format-time-string "%H:%M:%S+09:00")
         (format-time-string "%Y %b %d ")
      )
    )
    (write-file (expand-file-name file-path (concat mt3-site-location "parts")))
    (switch-to-buffer file-path)
    (auto-fill-mode)
  )
)

(defun thumbnail-path (image_url)
  (concat
   (file-name-directory image_url)
   "thumbs/"
   (file-name-nondirectory image_url)
   )
  )

(defun youtube-id (youtube_url)
  (car (last (split-string youtube_url "[/|=]")))
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

;; https://www.reddit.com/r/emacs/comments/6lzpre/use_interactive_list_readstring_to_read_arbitrary/
(defun journal-read-tags (tags)
  (let (tag done)
    (while (not done)
      (setq tag (read-string "Tag: "))
      (if (= (length tag) 0)
          (setq done t)
        (push tag tags)))
    (nreverse tags)))

;;;  http://stackoverflow.com/a/251922
(defvar current-date-time-format "##### %H:%M %A %d %B %Y %Z"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "##### %H:%M"
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
  (let ((file-name (journal-no-date-path title)))
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

(defun journal-no-date-path (title)
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

(global-set-key (kbd "C-c j") 'journal-new-post)
(global-set-key (kbd "C-c d") 'journal-new-dream-post)
(global-set-key (kbd "C-c n") 'journal-new-japanese-post)
(global-set-key (kbd "C-c r") 'ruby-tag-create-region)
(global-set-key (kbd "C-c b") 'blog-new-post)
(global-set-key (kbd "C-c e") 'event-new-post)
(global-set-key (kbd "C-c m") 'mt3-new-episode)
(global-set-key (kbd "C-c p") 'mt3-new-part)
(global-set-key (kbd "C-c y") 'hugo-youtube)
(global-set-key (kbd "C-c !") 'create-thumbs-with-b\.robnugen\.com-image-url-on-single-line)
(global-set-key (kbd "C-c 3") 'markdown-timestamp-short)
(global-set-key (kbd "C-c #") 'markdown-timestamp-full)

(provide 'journal)
;;; hugo.el ends here
