(defun test (&optional include-declaration &key display-action)
    (interactive "P")
    (findLocations "textDocument/references" nil :display-action display-action))

(cl-defun findLocations (method &optional extra &key display-action references?)
    (let ((loc (lsp-request method
                            (append (lsp--text-document-position-params) extra))))
        (setq data loc)
        (if (seq-empty-p loc)
            (lsp--error "Not found for: %s" (or (thing-at-point 'symbol t) ""))
            (lsp-show-xrefs items display-action references?)
            ;; (progn
            ;;     (setq items (lspLocationsToXrefItems loc))
            ;;     (if (eq (length '(items)) 1)
            ;;         (xref-quit-and-goto-xref items)))
            )))

(defun lspLocationsToXrefItems (locations)
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
    (indent-region (point-min)
                   (point-max))
    (delete-trailing-whitespace)
    ;;(delete-blank-lines)
    )

(defun rpl()
    (interactive)
    (setq currentPoint (point))
    (goto-char (point-min))
    (setq old (read-from-minibuffer "Old string: "))
    (setq new (read-from-minibuffer "New string: "))
    (while (re-search-forward old nil t)
        (replace-match new))
    (goto-char currentPoint))

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
