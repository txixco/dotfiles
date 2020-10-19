
;; General macros
;;
;; Author: txixco
;;

;;;; Keymaping
(global-set-key [(control f2)] 'fn-show-position)
(global-set-key [(control f4)] 'kill-this-buffer)
(global-set-key [(control f5)] 'fn-revert-buffer)
(global-set-key [(control f6)] 'fn-buffer-full-name)
(global-set-key [(control f7)] 'fn-remove-message-buffer)
(global-set-key [(control f8)] 'fn-get-char-code)
(global-set-key [(control f9)] 'fn-clean-comment)
(global-set-key [(control f11)] 'fn-ido-choose-from-recentf)

(global-set-key [(meta {)] 'fn-modify-brackets)
(global-set-key [(meta insert)] 'fn-kill-list)

(global-set-key (kbd "C-c j") 'org-remember)
(global-set-key (kbd "C-c .") 'fn-insert-date)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)
(global-set-key (kbd "C-c o") 'fn-json-oneliner)
(global-set-key (kbd "C-c a") 'my-increment-number-decimal)

(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") 'insert-pair)
(global-set-key (kbd "M-\"") 'insert-pair)

;; Variables
;(defvar af-current-alert 0)
;(defvar af-current-string "")
(defvar af-journal-file "E:/Dropbox/Seguridad/documentos/notas/diario.org")

(defvar af-unique-index 
  "CREATE UNIQUE INDEX %s.%s ON %s.%s
(%s)
TABLESPACE %s
NOLOGGING;\n\n")

(defvar af-primary-key
  "ALTER TABLE %s.%s ADD (
CONSTRAINT %s
   PRIMARY KEY (%s)
 USING INDEX);\n\n")

(defvar af-sql-comment
"/*
|| %s
||
|| Autor: Francisco J. Rueda %s
|| Validado:
||
*/\n")

(defvar af-date-format
  "%d/%m/%G %H:%M")

;; Functions
(defun fn-buffer-full-name (str)
  (interactive "bBuffer to ask: ")
  (setq buffer-full-name (buffer-file-name (get-file-buffer str)))
  (if (or (eq system-type 'windows-nt)
          (eq system-type 'ms-dos))
      (setq buffer-full-name 
            (replace-regexp-in-string "/" "\\\\" buffer-full-name)))
  (if current-prefix-arg
      (message "%s" buffer-full-name)
    (kill-new buffer-full-name)))

(defun fn-show-position (start end)
  (interactive "r")
  (setq long (- end start))
  (setq strline (1+ (- start (line-beginning-position))))
  (if current-prefix-arg
      (setq msg (format "Position is %d:%d" strline long))
    (setq msg (format "Length is %d" long)))
  (message msg))

(defun fn-insert-sql-comment (comment)
"Insert a SQL comment in the current buffer"
(interactive "sComment: ")
(insert (format af-sql-comment 
				comment 
				(format-time-string af-date-format))))

(defun fn-insert-date ()
"Insert the current date in the current buffer"
(interactive)
(insert (format-time-string af-date-format)))

(defun fn-modify-brackets (type start end)
  (interactive "cTipo de sustitución: \nr")
  (cond ((char-equal type 123) (replace-pairs-region start end '(["[" "{"]
															["]" "}"])))
		 ((char-equal type 91) (replace-pairs-region start end '(["{" "["]
														 ["}" "]"])))
		 ((char-equal type 34) (replace-pairs-region start end '(["“" "”"]
														 ["\"" "\""])))))

(defun fn-revert-buffer ()
  (interactive)
  (revert-buffer nil t t))

(defun fn-get-char-code (char)
  (interactive "cCarácter: ")
  (insert (number-to-string char)))

(defun fn-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string home "~" path))
                                  recentf-list)
                          nil t))))

(defun fn-titulize (title line-length)
  (interactive "sTítulo: \nnLongitud: ")
  (setq n (/ (- line-length (+ (length title) 2)) 2))
  (insert (concat (make-string line-length ?-) "\n"))
  (insert (concat (make-string n ?-) " " title " " (make-string n ?-) "\n"))
  (insert (concat (make-string line-length ?-) "\n")))

(defun fn-clean-comment (start end)
  (interactive "r")
  (setq text (buffer-substring start end))
  (setq text (replace-regexp-in-string "|| " "" text))
  (setq text (replace-regexp-in-string "\n" " " text))
  (setq text (replace-regexp-in-string "\s+" " " text))
  (kill-new text)
  (deactivate-mark))

(defun fn-kill-list (start end)
  (interactive "r")
  (setq text (concat ", " (buffer-substring start end)))
  (kill-append text nil)
  (deactivate-mark))

(defun fn-jbean (field type)
  (interactive "sField: \nsType: ")
  (setq upfield (upcase-initials field))
  (setq upfield (replace-regexp-in-string " " "" upfield))
  (insert (concat "public void set" upfield "(" type " " field ") {"))
  (indent-according-to-mode)
  (insert (concat "\nthis." field " = " field ";"))
  (indent-according-to-mode)
  (insert (concat "\n}"))
  (indent-according-to-mode)
  (insert (concat "\n\npublic " type " get" upfield "() {"))
  (indent-according-to-mode)
  (insert (concat "\nreturn this." field ";"))
  (indent-according-to-mode)
  (insert "\n}")
  (indent-according-to-mode)
  (insert "\n\n"))

(defun fn-json-onliner (start end)
  (interactive "r")
  (setq text (buffer-substring start end))
  (setq text (replace-regexp-in-string "[
	]+" "" text))
  (setq text (replace-regexp-in-string "  +" "" text))
  (kill-new text)
  (deactivate-mark))

(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (let ((remove (list (match-beginning 0) (match-end 0)))
        (description (if (match-end 3) 
                 (org-match-string-no-properties 3)
                 (org-match-string-no-properties 1))))
    (apply 'delete-region remove)
    (insert description))))

;; Keyboard macros
(fset 'fn-remove-message-buffer
   [?\C-x ?o ?\C-x ?k return ?\C-x ?o ?\C-x ?1])

