;; navigation
(global-set-key (kbd "<backtab>") 'custom/insert-tab)
(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-<down>") 'scroll-up-line)
(global-set-key (kbd "M-p") 'backward-page)
(global-set-key (kbd "M-n") 'forward-page)
(global-set-key (kbd "C-M-p") 'backwardParagraph)
(global-set-key (kbd "C-M-n") 'forwardParagraph)
(global-set-key (kbd "C-x C-g") 'goto-line-preview)
(global-set-key (kbd "M-o") 'switchNextBuffer)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-M-<return>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-<backspace>") 'mc/unmark-next-like-this)

(global-set-key (kbd "M-<left>") 'smart-shift-left)
(global-set-key (kbd "M-<up>") 'smart-shift-up)
(global-set-key (kbd "M-<down>") 'smart-shift-down)
(global-set-key (kbd "M-<right>") 'smart-shift-right)

;; flycheck
(global-set-key (kbd "C-<right>") 'flycheck-next-error)
(global-set-key (kbd "C-<left>") 'flycheck-previous-error)
(global-set-key (kbd "C-x r t") 'string-insert-rectangle)

(global-set-key (kbd "C-x s") 'custom/autoformat-sql)
(global-set-key (kbd "C-x j") 'custom/autoformat-json)

;; lsp
(global-set-key (kbd "C-M-.") 'lsp-goto-implementation)
;;(global-set-key (kbd "C-M-.") 'eglot-find-implementation)
(global-set-key (kbd "C-M-/") 'custom/lsp-goto-test)
(global-set-key (kbd "C-j") 'lsp-execute-code-action)
(global-set-key (kbd "C-M-j") 'ace-jump-char-mode)
(global-set-key (kbd "C->") 'mark-next-like-this)

;;(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
;;(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "RET") 'custom/newline-and-indent)

(defun generateHeadline ()
	(setq buffers (mapconcat (function buffer-name) (buffer-list) "aeyouv"))

	(setq ss (split-string buffers "aeyouv" t " "))

	(setq str "")
	(dolist (buffer-name ss)
		(if (not (string-match-p (regexp-quote "*") buffer-name))
			(progn
				(if (equal buffer-name (buffer-name))
					(put-text-property 0 (length buffer-name) 'face 'font-lock-warning-face
									   buffer-name))

				(setq str (concat str ";" buffer-name))
				)))
	(setq str (string-trim str " " " "))
	(setq str (string-trim str ";" ";"))
	(setq str (string-replace ";" " | " str)))

(setq header-line-format (generateHeadline))

(global-set-key (kbd "C-<tab>") 'switchPrevBuffer)
;;(global-set-key (kbd "C-<backspace>") 'switchPrevBuffer)
(define-key ibuffer-mode-map (kbd "C-<return>") 'ibuffer-visit-buffer)
(define-key ibuffer-mode-map (kbd "C-<tab>") 'kill-this-buffer)


(define-key ibuffer-mode-map (kbd "<backspace>") 'kill-this-buffer)
(define-key ibuffer-mode-map (kbd "C-k") 'kill-this-buffer)
(define-key ibuffer-mode-map (kbd "k") 'kill-this-buffer)
(define-key ibuffer-mode-map (kbd "h") 'ibuffer/toggle-default-buffers)

(global-set-key (kbd "C--") 'hs-hide-block)
(global-set-key (kbd "C-=") 'hs-show-block)

(global-set-key (kbd "M--") 'er/contract-region)
(global-set-key (kbd "M-=") 'er/expand-region)
(global-set-key (kbd "<f6>") 'imenu)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-k") 'custom-kill-line)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)
(global-set-key (kbd "M-d") 'forward-delete-word)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-r") 'replace-string)

(defun reverse-input-method (input-method)
	"Build the reverse mapping of single letters from INPUT-METHOD."
	(interactive
	 (list (read-input-method-name "Use input method (default current): ")))
	(if (and input-method (symbolp input-method))
		(setq input-method (symbol-name input-method)))
	(let ((current current-input-method)
		  (modifiers '(nil (control) (meta) (control meta))))
		(when input-method
			(activate-input-method input-method))
		(when (and current-input-method quail-keyboard-layout)
			(dolist (map (cdr (quail-map)))
				(let* ((to (car map))
					   (from (quail-get-translation
							  (cadr map) (char-to-string to) 1)))
					(when (and (characterp from) (characterp to))
						(dolist (mod modifiers)
							(define-key local-function-key-map
								(vector (append mod (list from)))
								(vector (append mod (list to)))))))))
		(when input-method
			(activate-input-method current))))

(reverse-input-method 'russian-computer)

(set-frame-size (selected-frame) 100 30)
