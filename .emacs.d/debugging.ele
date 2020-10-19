
;; Insert messages for debugging
;;
;; Author: txixco
;;

;; Keymaping

; Alerts
(global-set-key [(meta f3)]  'af-alert-string)
(global-set-key [(control f3)]  'af-alert-start)
(global-set-key [f3]  'af-alert-continue)

; Misc
(global-set-key (kbd "C-c f") 'af-search-function)

;; Variables
(defvar af-current-alert 0)
(defvar af-current-string "")

;; Functions

; Alerts
(defun af-make-string-javascript (str)
  (concat "alert("
          str
		  ");\n"))

(defun af-make-string-perl (str)
  (concat "print \""
          str
		  "\";\n"))

(defun af-make-string (str)
  (cond ((string= major-mode 'javascript-mode) (af-make-string-javascript str))
		((string= major-mode 'perl-mode) (af-make-string-perl str))
		(t 
		   (message "%s is not a valid mode" major-mode)
		   (eval ""))))

(defun af-alert (str)
  (insert (af-make-string str)))

(defun af-alert-start ()
  (interactive)
  (setq af-current-alert 0)
  (setq af-current-string "")
  (af-alert-continue))

(defun af-alert-continue ()
  (interactive)
  (cond ((string= af-current-string "")
		   (setq af-current-alert (1+ af-current-alert))
		   (setq str (number-to-string af-current-alert)))
		(t
		   (setq str af-current-string)))
  (af-alert str))

(defun af-alert-string (str)
  (interactive "sString to show: ")
  (setq af-current-string str)
  (af-alert af-current-string))


; Misc

(defun af-make-function-string (str)
  (cond ((string= major-mode 'javascript-mode) (concat "function " str))
		((string= major-mode 'perl-mode) (concat "sub " str))
		(t 
		   (message "%s is not a valid mode" major-mode)
		   (eval ""))))

(defun af-search-function (str)
  (interactive "sFunction to search for: ")
  (setq function-string (af-make-function-string str))
  (setq current-point (point))
  (beginning-of-buffer)
  (unless (search-forward function-string nil t)
	(goto-char current-point)))

; Load another files
(load "~/emacs/bookmarks")