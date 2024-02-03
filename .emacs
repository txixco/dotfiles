
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(load "~/.emacs.d/general")
(load "~/.emacs.d/xfrp_find_replace_pairs")

(add-to-list 'load-path "~/.emacs.d/lisp/")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :family "DejaVu Sans Mono" :foundry "outline"))))
 '(cperl-array-face ((t (:foreground "yellow4" :background "black"))) t)
 '(cperl-hash-face ((t (:foreground "yellow3" :background "black"))) t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-master nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(misterioso))
 '(custom-safe-themes
   '("3805a89afc4703132666650ca9f375ce445621cb8fde9c415ed59dcbd93733dd" "9962040a8c85f07b5030a9342763e34fa2e349570043fbde78e8ac56eb38c9d3" default))
 '(display-line-numbers-type 'relative)
 '(exec-path
   '("C:/Program Files/Aspell/bin/" "c:/Program Files (x86)/Common Files/Oracle/Java/javapath" "C:/ProgramData/Oracle/Java/javapath" "C:/windows/system32" "C:/windows" "C:/windows/System32/Wbem" "C:/windows/System32/WindowsPowerShell/v1.0/" "C:/Program Files/Cloud Foundry" "C:/Program Files/Git/cmd" "C:/Program Files/PuTTY/" "C:/Program Files/TortoiseGit/bin" "C:/Thanga/apache-maven-3.5.4/bin" "C:/Program Files/Java/jdk1.8.0_181/bin" "C:/Thanga/apache-maven-3.5.4/bin" "C:/Users/Thanga01/Downloads/spring-boot-cli-2.0.4.RELEASE-bin/spring-2.0.4.RELEASE/bin" "." "C:/Program Files/Microsoft VS Code/bin" "C:/Program Files/WinMerge" "C:/Users/frueda/AppData/Local/Programs/Python/Python37-32/Scripts/" "C:/Users/frueda/AppData/Local/Programs/Python/Python37-32/" "C:/Users/frueda/AppData/Local/Microsoft/WindowsApps" "C:/Users/frueda/AppData/Local/atom/bin" "C:/Users/frueda/AppData/Local/Pandoc/" "C:/Program Files/Salesforce CLI/bin" "c:/Programs/emacs/libexec/emacs/26.1/x86_64-w64-mingw32" "C:/Programs"))
 '(fill-column 80)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(package-selected-packages '(haskell-tng-mode haskell-mode org evil))
 '(safe-local-variable-values '((TeX-master . "ejb")))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode 'right)
 '(tab-width 4)
 '(text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))
 '(tool-bar-style 'image)
 '(warning-suppress-log-types '((comp) (comp) (comp)))
 '(warning-suppress-types '((comp) (comp))))
(setq-default abbrev-mode t)
(cond ((file-exists-p "~/.abbrev_defs")
       (read-abbrev-file "~/.abbrev_defs")))
(setq save-abbrevs t)
(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)


;; *******************************************************************
;; replace perl mode with cperl-mode
;;

(add-to-list 'auto-mode-alist '("\\.\\([gG]?[pP][Llm]\\|al\\)\\'" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
    (add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (setq cperl-indent-level 4)
  (setq cperl-close-paren-offset -4)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-tab-always-indent t)
  (setq cperl-extra-newline-before-brace t)
 (custom-set-faces
  '(cperl-array-face ((t (:foreground "yellow4" :background "black"))))
  '(cperl-hash-face ((t (:foreground "yellow3" :background "black"))))))


;; ...replacement done
;; *******************************************************************

; ---- language-env DON'T MODIFY THIS LINE!
;-*-emacs-lisp-*-
;;/etc/emacs/site-start.d/50user-es.el
;

;;--- soportar teclado europeo ------------------------------- 
(set-input-mode  (car (current-input-mode))
		 (nth 1 (current-input-mode))
		 0)
; `standard-display-european' is semi-obsolete and conflicts
; with multibyte characters. `set-language-environment' is
; a substitute.
; (standard-display-european t)

; no usar caracteres no-ascii (como acentos) como final de palabra
(if (>= emacs-major-version 20)
    (set-language-environment "Latin-1")
    (require 'iso-syntax))
(require 'disp-table)

;;--- redefinir algunas teclas ----------------------------------
;(global-set-key [backspace] 'backward-delete-char-untabify)
; la siguiente línea no debería romper el caracter de borrado
; en búsquedas incrementales - tiene esto otras desventajas?
;(global-set-key "\177" 'backward-delete-char-untabify)
;(global-set-key [delete] 'delete-char)
;(global-set-key [home] 'beginning-of-line)
;(global-set-key [end] 'end-of-line)

; Otras varias (gracias a Jose Manuel Moya)
;(defvar cursor-map-2 (make-keymap) "for ESC-[")
;(fset 'Cursor-Map-2 cursor-map-2)
;(define-key esc-map "[" 'Cursor-Map-2)
; 
;(define-key esc-map "[A" 'previous-line)
;(define-key esc-map "[B" 'next-line)
;(define-key esc-map "[C" 'forward-char)
;(define-key esc-map "[D" 'backward-char)
;(define-key esc-map "[H" 'beginning-of-line)
;(define-key esc-map "[Y" 'end-of-line)
;(define-key esc-map "[5^" 'scroll-down)
;(define-key esc-map "[6^" 'scroll-up)
;(define-key esc-map "[[A" 'help-for-help)
;(define-key esc-map "[[B" 'byte-compile-file)
;(define-key esc-map "[[C" 'isearch-forward)
;(define-key esc-map "[[D" 'query-replace-regexp)
;(define-key esc-map "[[E" 'eval-defun)
;(define-key esc-map "[[F" 'eval-current-buffer)
;(define-key esc-map "[[G" 'buffer-menu)
;(define-key esc-map "[[H" 'global-set-key)
;(define-key esc-map "[[I" 'save-buffer)
;(define-key esc-map "[[J" 'save-buffers-kill-emacs)
;(define-key esc-map "[2^" 'set-mark-command)
;(define-key esc-map "[3^" 'delete-char)

; entradas necesarias para XEmacs:
(global-set-key [(control home)] 'beginning-of-buffer)
(global-set-key [(control end)] 'end-of-buffer)

; ---- language-env end DON'T MODIFY THIS LINE!

;; autoload visual-basic-mode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode)) ; VBscript
(add-to-list 'auto-mode-alist '("\\.vb\\'" . visual-basic-mode))  ; visual basic
																  ; .NET file
(add-to-list 'auto-mode-alist '("\\.bas\\'" . visual-basic-mode)) ; visual basic
																  ; form
(add-to-list 'auto-mode-alist '("\\.frm\\'" . visual-basic-mode)) ; basic
																  ; language
																  ; source
(add-to-list 'auto-mode-alist '("\\.cls\\'" . visual-basic-mode)) ; C++ class
																  ; definition
                                        ; file

;; Aspell
(add-to-list 'exec-path "C:/Program Files/Aspell/bin/")
(setq ispell-program-name "aspell")
(setq ispell-personal-dictionary "~/.emacs.d/.ispell")
(require 'ispell)

; Cargar ~/.emacs.d/latex.el al iniciar el modo LaTex
(add-hook 'LaTeX-mode-hook 'fn-LaTeX-mode-hook)
(defun fn-LaTeX-mode-hook ()
  (load-file "~/.emacs.d/latex.el"))

(setq default-directory "~/Documents")

; recentf mode
(require 'recentf)
(recentf-mode 1)

;; ========== Enable Line and Column Numbering ==========

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;;;
;;; Org Mode
;;;

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Autohotkey mode
(autoload 'xahk-mode "xahk-mode" "Load xahk-mode for editing AutoHotkey scripts." t)
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

;; Journaling
;(require 'remember)
;(require 'org-remember)
;(org-remember-insinuate)
;(setq org-remember-templates
;      '(("Incidencias"
;    	 ?i
;    	 "* %U %? %^g\n %c \n"
;    	 "C:/Data/Seguridad/documentos/notas/diario.org"
;    	 "Incidencias")
;        ("Asistencia"
;    	 ?a
;    	 "* %u %? \n"
;    	 "C:/Data/Seguridad/documentos/notas/diario.org"
;    	 "Asistencia")))

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Beautify org-mode
;(require 'org-bullets)
;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; Some other modes

(add-to-list 'auto-mode-alist '("\\.\\(sql\\|fnc\\)$" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.cmp\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
    (autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
    (autoload 'css-mode "css-mode" nil t)

;; Tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; Powerline
(require 'powerline)
(powerline-default-theme)

;; Evil
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; Haskell
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook #'hindent-mode)
