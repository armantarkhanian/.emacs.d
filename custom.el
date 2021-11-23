(defun find-file-as-root ()
    (interactive)
    (setq fileName (read-file-name "Find file: "))

    (setq fullFileName (expand-file-name fileName))
    (setq rootFile (concat "/su::" fullFileName))
    (find-file rootFile))

(defun calc-region (point mark)
    (interactive "r")
    (setq result (calc-eval
                  (buffer-substring
                   point
                   mark)))
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
    (find-file "~/.emacs.d/init.el"))

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
