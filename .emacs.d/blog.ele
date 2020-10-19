;; Some defined keyboard macros for blogging
;;
;; Author: txixco
;;

;;;; Keymaping
(global-set-key (kbd "C-c a")  'af-blog-abbr )
(global-set-key (kbd "C-c f")  'af-fdt )
(global-set-key (kbd "C-c i")  'af-blog-img )
(global-set-key (kbd "C-c n")  'af-note )
(global-set-key (kbd "C-c p")  'af-paragraph )
(global-set-key (kbd "C-c q")  'af-blockquote )
(global-set-key (kbd "C-c r")  'af-blog-href )
(global-set-key (kbd "C-c s")  'af-strike )

(defun af-get-region-string ()
  (interactive)
  (setq str "")
  (if mark-active
	  (setq str (buffer-substring (region-beginning) (region-end))))
  str)

(defun af-delete-region ()
  (interactive)
  (if mark-active
	  (delete-region (region-beginning) (region-end))))

(defun af-blog-href (href)
  "Writes \"a\" tag, asking for href."
  (interactive "*shref: \n")
  (setq region (af-get-region-string))
  (af-delete-region)  
  (insert (concat "<a href='"
				  href
				  "'>"
				  region
				  "</a>"))
  (if (string= region "")
	  (backward-char 4)))

(defun af-blog-abbr (title)
  "Writes \"abbrev\" tag, asking for title"
  (interactive "*stitle: \n")
  (setq region (af-get-region-string))
  (af-delete-region)  
  (insert (concat "<abbr title='"
				  title
				  "'>"
				  region
				  "</abbr>"))
  (if (string= region "")
	  (backward-char 7)))

(defun af-strike ()
  "Writes \"abbrev\" tag, asking for title"
  (interactive)
  (setq region (af-get-region-string))
  (af-delete-region)  
  (insert (concat "<strike>"
				  region
				  "</strike>"))
  (if (string= region "")
	  (backward-char 9)))

(defun af-paragraph ()
  "Writes \"<p></p>\" and set the point to start writing."
  (interactive)
  (insert "<p></p>")
  (backward-char 4))

(defun af-blockquote ()
  "Writes \"<p></p>\" and set the point to start writing."
  (interactive)
  (insert "\n<blockquote>\n\t\n</blockquote>")
  (backward-char 14))

(defun af-fdt ()
  "Writes \"<div class='FdT'></div>\" and set the point to start writing."
  (interactive)
  (insert "<div class='FdT'>\n\n</div>")
  (backward-char 7))

(defun af-note ()
  "Writes \"<div class='nota'></div>\" and set the point to start writing."
  (interactive)
  (insert "<div class='nota'>\n\n</div>")
  (backward-char 7))

(defun af-blog-imgr (alt src)
  "Writes a img tag, right aligned"
  (insert (concat "<div style='float: right;'>\n"
				  "<img style='margin: 0pt 0pt 0pt 10pt;' src='"
				  src
				  "' alt='"
				  alt
				  "' />\n"
				  "</div>")))

(defun af-blog-imgl (alt src)
  "Writes a img tag, left aligned, asking for alt attribute"
  (insert (concat "<div style='float: left;'>\n"
				  "<img style='margin: 0pt 10pt 0pt 0pt;' src='"
				  src
				  "' alt='"
				  alt
				  "' />\n"
				  "</div>")))

(defun af-blog-imgc (alt src)
  "Writes a img tag, centered"
  (insert (concat "<div style='float: center;'>\n"
				  "<img style='margin: 0pt;' src='"
				  src
				  "' alt='"
				  alt
				  "' />\n"
				  "</div>")))

(defun af-blog-img (alt align)
  (interactive "*salt: \ncAlignment (c/l/r): ")
  (setq region (af-get-region-string))
  (af-delete-region)  
  (cond ((char-equal align ?c)
		(af-blog-imgc alt region))
		((char-equal align ?l)
		(af-blog-imgl alt region))
		((char-equal align ?r)
		(af-blog-imgr alt region))
        (t (message "Alignment is not valid!"))))


