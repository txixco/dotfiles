;; Some defined keyboard macros for LaTeX
;;
;; Author: txixco
;;

;; Keymaping
(global-set-key (kbd "C-c c")  'fn-wrap-command)
(global-set-key (kbd "C-c t")  'fn-insert-table)
(global-set-key (kbd "C-c f")  'fn-insert-figure)

(defun fn-wrap-command (command)
  (interactive "sComando: ")
  (save-excursion 
    (goto-char (region-end)) (insert "}"))
    (goto-char (region-beginning)) (insert (concat "\\" command "{")))

(defun fn-insert-indent (string)
  (insert string)
  (newline-and-indent))

(defun fn-get-name (description)
  (replace-regexp-in-string " " "" (capitalize description)))

(defun fn-insert-table-item (row columns)
  (indent-according-to-mode)
  (if (= (% row 2) 0)
	  (insert "\\OddRow ")
	  (insert "\\EvenRow "))
  
  (fn-insert-indent (concat (read-string (concat "Columna 1 de la fila " 
									   (number-to-string row) 
									   ": "))))
  
  (let ((i 2))
	(while (<= i columns)
	  (insert (concat "& " (read-string (concat "Columna " 
												   (number-to-string i) 
												   " de la fila " 
												   (number-to-string row) 
												   ": "))
					  " "))
	  (if (= i columns)
		  (fn-insert-indent "\\\\")
		(newline-and-indent))
	  (setq i (1+ i)))))

(defun fn-insert-table (columns rows description)
  (interactive "nNúmero de columnas: \nnNúmero de filas: \nsDescripción: ")
  (insert "\\begin{WideTabular}{+l")

  (let ((i 2))
	(while (<= i columns)
	  (insert "|=l")
	  (setq i (1+ i)))
	(fn-insert-indent (concat "}{"
							  description
							  "}{"
							  (fn-get-name description)
							  "}"))

	(insert "\\TableHead ")
	(setq i 1)
	(while (<= i columns)
	  (insert (read-string (concat "Columna " (number-to-string i) ": ")))
	  (if (= i columns)
		  (fn-insert-indent " \\\\")
	    (insert " & "))
	  (setq i (1+ i)))

	(setq i 1)
	(while (<= i rows)
	  (fn-insert-table-item i columns)
	  (setq i (1+ i))))

  (insert "\\end{WideTabular}")
  (indent-according-to-mode)
  (newline-and-indent))


(defun fn-insert-figure (description)
  (interactive "sDescripción: ")
  (setq figure-name (fn-get-name description))
  (insert (concat "\\Figure{" figure-name "}{" description "}\n")))

;; Local Variables:
;; coding: utf-8-unix
;; End:
