(require 'package)
(setq package-archives
      '(
        ("gnu" . "https://elpa.gnu.org/packages/")
	    ("melpa-stb" . "https://stable.melpa.org/packages/")
	    ("melpa" . "https://melpa.org/packages/")
        ))
(package-initialize)
(unless (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)
    (setq use-package-always-ensure t))

(when window-system (set-frame-size (selected-frame) 110 32))
(setq warning-minimum-level :emergency)

(use-package yasnippet
    :ensure t 
    :commands yas-minor-mode
    :hook (go-mode . yas-minor-mode))

(use-package yasnippet-snippets
    :ensure t)

(use-package all-the-icons
    :ensure t    
    (all-the-icons-install-fonts))

(use-package all-the-icons-dired
    :ensure t)

(use-package neotree
    :ensure t)

(use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (setq doom-themes-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    (doom-themes-config)
    )

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(load-theme 'doom-snazzy t)

(use-package lsp-mode
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred)
    :hook (python-mode . lsp-deferred)
    :hook (dart-mode . lsp)
    :config (progn
                ;; use flycheck, not flymake
                (setq lsp-prefer-flymake nil)))

(use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)


(show-paren-mode 1)
(global-hl-line-mode 1) ;; Подсвечивать текущую строку
;;(set-face-background 'hl-line "#ccc")
;;(set-face-foreground 'highlight nil)


(global-display-line-numbers-mode)
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(global-set-key (kbd "RET") 'newline-and-indent)

(defvar --backup-directory "~/.emacs.d/backups")
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(defun backups()
    (interactive)
    (find-file "~/.emacs.d/backups")
    )

(add-hook 'prog-mode-hook #'hs-minor-mode)

(global-set-key (kbd "C--") 'hs-hide-block)
(global-set-key (kbd "C-=") 'hs-show-block)

(require 'imenu)
(setq imenu-auto-rescan      t)
(setq imenu-use-popup-menu nil)
(global-set-key (kbd "<f6>") 'imenu)
(setq frame-title-format "GNU Emacs: %b")
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)
(show-paren-mode t)
(setq show-paren-style 'expression)
(electric-pair-mode    1)
(electric-indent-mode  1)
(delete-selection-mode t)
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(scroll-bar-mode   -1)
(blink-cursor-mode 1)
(setq use-dialog-box     nil)
(setq redisplay-dont-pause t)
(setq ring-bell-function 'ignore)
(set-language-environment 'UTF-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read    'utf-8)
(setq file-name-coding-system           'utf-8)
(set-selection-coding-system            'utf-8)
(set-keyboard-coding-system        'utf-8-unix)
(set-terminal-coding-system             'utf-8)
(prefer-coding-system                   'utf-8)
(fringe-mode '(0 . 0))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(add-hook 'yaml-mode-hook
          (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(add-hook 'dired-mode-hook
          (lambda()
              (define-key dired-mode-map "b" 'dired-up-directory)
              (dired-hide-details-mode)
              ))

(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries 'left)
(set-face-attribute 'default nil :font "Monospace Bold 12")
(setq display-time-24hr-format t)
(display-time-mode             t)
(size-indication-mode          t)
(setq word-wrap          t)
(global-visual-line-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)
(setq lisp-indent-function  'common-lisp-indent-function)
(setq scroll-step               1)
(setq scroll-margin             5)
(setq scroll-conservatively 10000)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)

(defun gotoUp ()
    (interactive)
    (goto-char (point-max))
    )

(defun gotoDown ()
    (interactive)
    (goto-char (point-min))
    )

(defun run ()
    (interactive)
    (compile (concat "go run " (buffer-file-name)))
    )

(defun tidy ()
    (interactive)
    (compile "go mod tidy")
    )

(defun build ()
    (interactive)
    (compile "go build")
    )

(defun install ()
    (interactive)
    (compile "go install")
    )

(defun conf()
    (interactive)
    (find-file "~/.emacs.d/init.el")
    )

(defun ful ()
    (interactive)
    (toggle-frame-fullscreen)
    )

(defun backward-delete-word (arg)
    "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
    (interactive "p")
    (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "M-n") 'gotoUp)
(global-set-key (kbd "M-p") 'gotoDown)
(global-set-key (kbd "C-j") 'lsp-execute-code-action)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "C-r") 'replace-string)
;;; end of main config

;; optional package to get the error squiggles as you edit
(use-package flycheck
    :ensure t)

;;; For modes using [company](https://company-mode.github.io/) for tab
;;; completion add
(use-package company
    :init
    (setq company-idle-delay t  ; avoid auto completion popup, use TAB
                                        ; to show it
	      company-tooltip-align-annotations t)
    :hook (after-init . global-company-mode)
    :bind
    (:map prog-mode-map
	      ("C-i" . company-indent-or-complete-common)
	      ("C-M-i" . counsel-company)))

(use-package go-mode
    :ensure t)

(defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'flycheck-mode)

(add-hook 'before-save-hook 'fmt)


(defun fmt ()
    (interactive)    
    (indent-region (point-min) (point-max))
    )

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
    :ensure t
    :config
    ;; Optionally enable completion-as-you-type behavior.
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1))

(use-package web-mode
    :ensure t
    :mode "\\.html\\'"
    :mode "\\.js\\'"
    :mode "\\.vue\\'"
    :mode "\\.json\\'")

(use-package protobuf-mode
    :ensure t)

(font-lock-add-keywords 'go-mode      '(("\\<\\(\\|int\\|string\\|bool\\|byte\\|complex\\|complex64\\|complex128\\|uint16\\|false\\|float32\\|float64\\|int\\|int8\\|int16\\|uint32\\|int32\\|int64\\|iota\\|uint64\\|true\\|uint\\|uint8\\|uintptr\\)\\>" . font-lock-type-face)))


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

(defun dir ()
    (interactive)
    (find-file default-directory)
    )

(require 'subr-x)

(defun vue-new-component ()
    (interactive)
    (setq projectFolder (read-directory-name "Enter path to  /src folder of Vue-js project: "))
    (setq projectFolder (string-remove-suffix "/" projectFolder))
    (setq projectFolder (concat projectFolder "/components"))
    (setq componentName (read-from-minibuffer "New component name: "))
    (setq dir (expand-file-name (concat (file-name-as-directory projectFolder) componentName)))
    (make-directory dir)
    (setq templateFile (concat dir "/" componentName ".vue"))
    (setq scriptFile (concat dir "/script.js"))
    (setq styleFile (concat dir "/style.css"))
    (setq scriptFileContent (concat "export default {
    name: '" componentName "'
}
"))
    (setq templateFileContent (concat "<template>
    <div>
        " componentName " component" "
    </div>
</template>

<script src=\"./script.js\"></script>

<style src=\"./style.css\" scoped></style>
"))
    (write-region scriptFileContent nil scriptFile)
    (write-region "" nil styleFile)
    (write-region templateFileContent nil templateFile)
    )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-eldoc-enable-hover nil)
 '(lsp-go-analyses
   (quote
    (("fieldalignment" . t)
     ("unusedparams" . t)
     ("assign" . t)
     ("unusedwrite" . t))))
 '(lsp-go-hover-kind "FullDocumentation")
 '(lsp-go-use-gofumpt t)
 '(lsp-ui-doc-max-height 30)
 '(lsp-ui-doc-max-width 60)
 '(package-selected-packages
   (quote
    (vue-mode multi-web-mode lsp-python-ms protobuf-mode web-mode go-mode company flycheck lsp-ui lsp-mode doom-themes neotree all-the-icons-dired yasnippet-snippets yasnippet use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
