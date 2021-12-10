(defun custom/newline-and-indent ()
	(interactive)

	(if (region-active-p) (delete-region (region-beginning) (region-end)))

	(if (and (eq major-mode 'go-mode) (nth 3 (syntax-ppss)))
		(progn
			(setq-local currentPos (point))
			(beginning-of-line)
			(setq-local lineStartPoint (point))
			(end-of-line)
			(setq-local lineEndPoint (point))

			(setq-local tabsCount (count-sub (buffer-substring lineStartPoint lineEndPoint) "	"))
			(goto-char currentPos)
			(setq-local str "")
			(while (not (eq tabsCount 0))
				(setq-local str (concat str "	"))
				(setq-local tabsCount (- tabsCount 1))
				)
			(newline)
			(insert str)
			)
		(progn
			(reindent-then-newline-and-indent)
			)))



(defun cmdfd ()
	(interactive)
	(with-current-buffer "*Shell Command Output*"
		(progn

			))
	(insert output)
	)

(defun custom/buffer-content (buffer-name)
	(with-current-buffer buffer-name (buffer-string)))


(defun custom/shell-command (command)
	(with-temp-buffer
		(setq exitCode (call-process "/bin/bash" nil (current-buffer) nil "-c" command))
		(setq result '())
		(add-to-list 'result (buffer-string))
		(add-to-list 'result exitCode)))

(defun custom/format-sql (point mark)
	(interactive "r")
	(setq-local query (buffer-substring point mark))
	(setq-local queryCommand (concat "echo \"" query "\" | pg_format -T -g"))
	(setq-local output1 (shell-command-to-string queryCommand))
	(if (region-active-p) (delete-region (region-beginning) (region-end)))
	(insert output1))

(defun custom/format-sql-buffer ()
	(interactive)
	(setq-local savePoint (point))
	(setq-local query (buffer-substring (point-min) (point-max)))
	(setq-local queryCommand (concat "echo \"" query "\" | pg_format -T -g"))
	(setq-local output1 (shell-command-to-string queryCommand))
	(delete-region (point-min) (point-max))
	(insert output1)
	(goto-char savePoint))

(defun custom/autoformat-sql ()
	(interactive)
	(setq wholeStart (point))
	(beginning-of-line)
	(open-line 1)
	(insert "-- emacsautoformatmode")
	(search-backward "`")
	(fmt)
	(setq-local currentPos (point))
	(goto-char (- (point) 1))

	(beginning-of-line)
	(setq-local lineStartPoint (point))
	(end-of-line)
	(setq-local lineEndPoint (point))

	(setq-local tabsCount (+ (count-sub (buffer-substring lineStartPoint lineEndPoint) "	") 1))

	(beginning-of-line)

	(search-forward "`")
	(setq-local start (point))

	(search-forward "`")
	(setq-local end (- (point) 1))

	(setq-local query (buffer-substring start end))
	(setq-local queryCommand (concat "echo \"" query "\" | pg_format -T -g"))
	(setq-local queryCommand (concat queryCommand "\n"))

	(setq result (custom/shell-command queryCommand))
	(setq exitCode (pop result))
	(setq output1 (pop result))

	(if (not (eq exitCode 0))
		(progn
			(goto-char lineStartPoint)
			(replace-string "\"emacsautoformatmode\": \"\"," "")
			(kill-line)
			(goto-char wholeStart)
			(message output)
			)
		(progn
			(delete-region start end)
			(goto-char (- (point) 1))
			(insert (concat "
" output1))
			(delete-backward-char 1)
			(goto-char currentPos)
			(end-of-line)
			(goto-char (+ (point) 1))
			(setq-local start (point))
			(search-forward "`")
			(setq-local end (point))
			(setq-local str "")
			(while (not (eq tabsCount 0))
				(setq-local str (concat str "	"))
				(setq-local tabsCount (- tabsCount 1))
				)
			(string-insert-rectangle start end str)
			(goto-char (- (point) 1))
			(delete-backward-char 1)
			(search-backward "`")
			(end-of-line)
			(replace-string "-- emacsautoformatmode" "")
			(setq-local deleteStart (point))
			(beginning-of-line)
			(backward-char 1)
			(setq-local deleteEnd (point))
			(delete-region deleteStart deleteEnd)
			(next-line)
			)))

(defun custom/format-json (point mark)
	(interactive "r")
	(setq-local query (buffer-substring point mark))
	(setq-local queryCommand (concat "echo '" query "' | jq --tab"))
	(setq-local queryCommand (concat queryCommand "\n"))
	(setq-local output1 (shell-command-to-string queryCommand))
	(if (region-active-p) (delete-region (region-beginning) (region-end)))
	(insert output1))

(defun custom/autoformat-json ()
	(interactive)
	(setq wholeStart (point))
	(beginning-of-line)
	(open-line 1)
	(insert "\"emacsautoformatmode\": \"\",")
	(search-backward "`")
	(fmt)
	(setq-local currentPos (point))
	(goto-char (- (point) 1))

	(beginning-of-line)
	(setq-local lineStartPoint (point))
	(end-of-line)
	(setq-local lineEndPoint (point))

	(setq-local tabsCount (count-sub (buffer-substring lineStartPoint lineEndPoint) "	"))

	(beginning-of-line)

	(search-forward "`")
	(setq-local start (point))

	(search-forward "`")
	(setq-local end (- (point) 1))

	(setq-local query (buffer-substring start end))
	(setq-local queryCommand (concat "echo '" query "' | jq --tab"))
	(setq-local queryCommand (concat queryCommand "\n"))

	(setq result (custom/shell-command queryCommand))
	(setq exitCode (pop result))
	(setq output1 (pop result))

	(if (not (eq exitCode 0))
		(progn
			(goto-char lineStartPoint)
			(replace-string "\"emacsautoformatmode\": \"\"," "")
			(kill-line)
			(goto-char wholeStart)
			(message output)
			)
		(progn
			(delete-region start end)
			(goto-char (- (point) 1))
			(insert output1)
			(delete-backward-char 1)
			(goto-char currentPos)
			(end-of-line)
			(goto-char (+ (point) 1))
			(setq-local start (point))
			(search-forward "`")
			(setq-local end (point))
			(setq-local str "")
			(while (not (eq tabsCount 0))
				(setq-local str (concat str "	"))
				(setq-local tabsCount (- tabsCount 1))
				)
			(string-insert-rectangle start end str)
			(goto-char (- (point) 1))
			(search-backward "`")
			(end-of-line)
			(replace-string "\"emacsautoformatmode\": \"\"," "")
			(setq-local deleteStart (point))
			(beginning-of-line)
			(backward-char 1)
			(setq-local deleteEnd (point))
			(delete-region deleteStart deleteEnd)
			(next-line))))

(defun custom/count-tabs-in-line ()
	(interactive)
	(beginning-of-line)
	(setq-local lineStartPoint (point))
	(end-of-line)
	(setq-local lineEndPoint (point))

	(setq-local lineContent (buffer-substring lineStartPoint lineEndPoint))
	(setq-local tabsInPreviousLine (count-sub lineContent "	"))
	)

(defun custom/format-json-buffer ()
	(interactive)
	(setq-local savePoint (point))
	(setq-local query (buffer-substring (point-min) (point-max)))
	(setq-local queryCommand (concat "echo '" query "' | jq --tab"))
	(setq-local queryCommand (concat queryCommand "\n"))
	(setq-local output1 (shell-command-to-string queryCommand))
	(delete-region (point-min) (point-max))
	(insert output1)
	(goto-char savePoint))

(defun custom/insert-tab ()
	(interactive)
	(insert "	"))

;; (count-sub "			\"userID\": \"Новинки\"," "	")

(defun count-sub-1 (str pat)
	(loop with z = 0 with s = 0 while s do
		  (when (setf s (search pat str :start2 s)) ;; :start6 typo fixed
			  (incf z) (incf s (length pat)))
		  finally (return z)))

(defun count-sub (str &rest patterns)
	(reduce #'+ patterns :key (lambda (item) (count-sub-1 str item))))

(defun no ()
	(interactive)
	(find-file "~/.emacs.d/notes"))

(defun custom/lsp-goto-test (&optional include-declaration &key display-action)
	(interactive "P")
	(custom/lsp-find-references "textDocument/references" nil :display-action display-action))

(defun custom/lsp-find-references (method &optional extra &key display-action references?)
	(let ((loc (lsp-request method
							(append (lsp--text-document-position-params) extra))))
		(setq data loc)
		(if (seq-empty-p loc)
			(lsp--error "Not found for: %s" (or (thing-at-point 'symbol t) ""))
			(progn
				(setq items (custom/lsp-locations-to-xref-items loc))
				(lsp-show-xrefs items display-action references?)))))

(defun custom/lsp-locations-to-xref-items (locations)
	"Return a list of `xref-item' given LOCATIONS, which can be of
type Location, LocationLink, Location[] or LocationLink[]."
	(setq locations
		  (pcase locations
			  ((seq (or (Location)
						(LocationLink)))
			   (append locations nil))
			  ((or (Location)
				   (LocationLink))
			   (list locations))))

	(cl-labels ((get-xrefs-in-file
					(file-locs)
					(-let [(filename . matches) file-locs]
						(if (string-suffix-p "_test.go" filename)
							(progn
								(condition-case err
									(let ((visiting (find-buffer-visiting filename))
										  (fn (lambda (loc)
												  (lsp-with-filename filename
													  (lsp--xref-make-item filename
																		   (lsp--location-range loc))))))
										(if visiting
											(with-current-buffer visiting
												(seq-map fn matches))
											(when (file-readable-p filename)
												(with-temp-buffer
													(insert-file-contents-literally filename)
													(seq-map fn matches)))))
									(error (lsp-warn "Failed to process xref entry for filename '%s': %s"
													 filename (error-message-string err)))
									(file-error (lsp-warn "Failed to process xref entry, file-error, '%s': %s"
														  filename (error-message-string err)))))
							))))

		(->> locations
			 (seq-sort #'lsp--location-before-p)
			 (seq-group-by (-compose #'lsp--uri-to-path #'lsp--location-uri))
			 (seq-map #'get-xrefs-in-file)
			 (apply #'nconc))))

(defun mock()
	(interactive)
	(shell-command "gomocker"))

(setq default-buffers-shown nil)

(defun ibufferVisitBuffer (&optional single)
	(interactive "P")
	(ibuffer-visit-buffer single))
;;(kill-buffer "*Ibuffer*"))

(defun ibuffer/toggle-default-buffers ()
	(interactive)
	(ibuffer-filter-disable)
	(if default-buffers-shown
		(ibuffer-filter-by-name "^[^\*]")
		(ibuffer-filter-by-name ""))

	(setq default-buffers-shown (not default-buffers-shown)))

(defun share (point mark)
	(interactive "r")
	(webpaste--paste-text (buffer-substring point mark)))

(defun config-save ()
	(interactive)
	(shell-command "/bin/bash ~/.emacs.d/push"))

(defun find-file-as-root ()
	(interactive)
	(setq fileName (read-file-name "Find file: "))

	(setq fullFileName (expand-file-name fileName))
	(setq rootFile (concat "/su::" fullFileName))
	(find-file rootFile))

(defun calc-region (point mark)
	(interactive "r")
	(setq result (calc-eval (buffer-substring point mark)))
	(insert (concat " = " result)))

(defun switchNextBuffer()
	(interactive)
	(switch-to-next-buffer)
	(setq name (buffer-name))
	(if (equal "*" (substring name 0 1))
		(switchNextBuffer)))

(defun md()
	(interactive)
	(if (equal (buffer-name) "README.html")
		(saveREADME)))

(defun saveREADME()
	(setq s (buffer-substring (point-min) (point-max)))
	(setq s1 (html-to-markdown-string s))
	(write-region s1 nil "./README.md"))

(defun backups()
	(interactive)
	(find-file "~/.emacs.d/backups"))

(defun gotoUp ()
	(interactive)
	(goto-char (point-max)))

(defun gotoDown ()
	(interactive)
	(goto-char (point-min)))

(defun run ()
	(interactive)
	(compile (concat "go run " (buffer-file-name))))

(defun tidy ()
	(interactive)
	(compile "go mod tidy"))

(defun build ()
	(interactive)
	(compile "go build"))

(defun install ()
	(interactive)
	(compile "go install"))

(defun conf()
	(interactive)
	(if (display-graphic-p)
		(find-file "~/.emacs.d/init_window.el")
		(find-file "~/.emacs.d/init_nw.el")))

(defun hotkeys()
	(interactive)
	(find-file "~/.emacs.d/keybindings.el"))

(defun custom()
	(interactive)
	(find-file "~/.emacs.d/custom.el"))

(defun ful ()
	(interactive)
	(toggle-frame-fullscreen))

(defun backward-delete-word (arg)
	(interactive "p")
	(delete-region (point)
				   (progn (backward-word arg)
						  (point))))

(defun forward-delete-word (arg)
	(interactive "p")
	(delete-region (point)
				   (progn (forward-word arg)
						  (point))))

(defun custom-kill-line ()
	(interactive)
	(delete-region (point)
				   (line-end-position)))
(defun my-delete-line ()
	"Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
	(interactive)
	(delete-region (point)
				   (progn (end-of-line)
						  (point))))

(defun upBlock()
	(interactive)
	(re-search-backward "[}{]" nil t 1))

(defun downBlock()
	(interactive)
	(re-search-forward "[{}]" nil t 1))
(defun backwardParagraph()
	(interactive)
	(previous-line)
	(previous-line)
	(previous-line)
	(previous-line)
	(previous-line)
	(previous-line)
	(previous-line))


(defun forwardParagraph()
	(interactive)
	(next-line)
	(next-line)
	(next-line)
	(next-line)
	(next-line)
	(next-line)
	(next-line))

(defun fmt ()
	(interactive)

	(when
		(and
		 (not (eq major-mode 'yaml-mode))
		 (not (eq major-mode 'sql-mode)))
		(indent-region (point-min) (point-max))
		(delete-trailing-whitespace))

	(when (eq major-mode 'sql-mode)
		(custom/format-sql-buffer))

	(when (eq major-mode 'json-mode)
		(custom/format-json-buffer)))

(defun rpl()
	(interactive)
	(setq currentPoint (point))
	(goto-char (point-min))
	(setq old (read-from-minibuffer "Old string: "))
	(setq new (read-from-minibuffer "New string: "))
	(while (re-search-forward old nil t)
		(replace-match new))
	(goto-char currentPoint))

(defun custom/insert-tabs ()
	(interactive)

	(setq tabsCount (string-to-number (read-from-minibuffer "Tabs count: ")))

	(setq str "")

	(while (not (eq tabsCount 0))
		(setq str (concat str "	"))
		(setq tabsCount (- tabsCount 1)))

	(if (not (eq str ""))
		(string-insert-rectangle (point) (mark )str))

	)

(defun new-vue-component ()
	(interactive)
	(setq projectFolder (read-directory-name "Enter path to /components directory: "))
	(setq projectFolder (string-remove-suffix "/" projectFolder))
	(setq componentName (read-from-minibuffer "New component name: "))
	(setq dir (expand-file-name (concat (file-name-as-directory projectFolder) componentName)))
	(make-directory dir)
	(setq templateFile (concat dir "/" componentName ".vue"))
	(setq scriptFileContent (concat "export default {
    name: '" componentName "'
}
"))
	(setq templateFileContent (concat "<template>
    <div>
        " componentName " component" "
    </div>
</template>

<script>
    export default {
        name: '" componentName "'
    }
</script>

<style scoped>
* {

}
</style>
"))
	(write-region templateFileContent nil templateFile)
	(find-file templateFile))
