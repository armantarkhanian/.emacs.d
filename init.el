(require 'package)
(setq package-archives
      '(
        ("melpa-stb" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
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

(use-package highlight-indent-guides
    :ensure t)

(use-package magit
    :ensure t)

(defun my-highlighter (level responsive display)
    (if (> 1 level)
        nil
        (highlight-indent-guides--highlighter-default level responsive display)))

(setq highlight-indent-guides-highlighter-function 'my-highlighter)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package yasnippet-snippets
    :ensure t)

(use-package all-the-icons
    :ensure t
    (all-the-icons-install-fonts))

(use-package goto-line-preview
    :ensure t)

;; (use-package doom-modeline
;;     :ensure t
;;     :init (doom-modeline-mode 1))
;; (setq doom-modeline-minor-modes nil)

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

;;(load-theme 'vs-light t)
(use-package vscode-dark-plus-theme
    :ensure t
    :config
    (load-theme 'vscode-dark-plus t))

(setq vscode-dark-plus-invert-hl-todo nil)


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

(use-package flycheck-golangci-lint
    :ensure t)

;; (set-face-attribute 'region nil :background "#666" :foreground "#ffffff")
(set-face-attribute 'region nil :background "#333" :foreground nil)
(eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

(show-paren-mode 1)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#888")
(set-face-attribute 'show-paren-match nil :weight 'regular)

;;((t
;;  (:weight regular :foreground "#888" :background "#1e1e1e")))

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

(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "C-<right>") 'flycheck-next-error)
(global-set-key (kbd "C-<left>") 'flycheck-previous-error)

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
(set-face-attribute 'default nil :font "Monospace 12")
;;(set-face-attribute 'default nil :font "Hack 12")
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
    (interactive "p")
    (delete-region (point) (progn (backward-word arg) (point))))

(defun forward-delete-word (arg)
    (interactive "p")
    (delete-region (point) (progn (forward-word arg) (point))))

(defun custom-kill-line ()
    (interactive)
    (delete-region (point) (line-end-position))
    (delete-char 1)
    )
(defun my-delete-line ()
    "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
    (interactive)
    (delete-region (point) (progn (end-of-line) (point))))


(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-k") 'custom-kill-line)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)
(global-set-key (kbd "M-d") 'forward-delete-word)
(global-set-key (kbd "M-p") 'backward-page)
(global-set-key (kbd "M-n") 'forward-page)
(global-set-key (kbd "C-M-p") 'backward-paragraph)
(global-set-key (kbd "C-M-n") 'forward-paragraph)
(global-set-key (kbd "C-M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j") 'lsp-execute-code-action)
(global-set-key (kbd "C-x C-g") 'goto-line-preview)
(global-set-key (kbd "C-r") 'replace-string)

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
    (goto-char currentPoint)
    )

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
    :ensure t
    :config
    ;; Optionally enable completion-as-you-type behavior.
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1))

(use-package multi-web-mode
    :ensure
    :mode "\\.vue\\'"
    :config
    (setq mweb-default-major-mode 'web-mode)
    (setq mweb-tags '(
                      (css-mode "<style.*>" "</style>")
                      ))
    (setq mweb-filename-extensions '("vue" "html"))
    )

(multi-web-global-mode 1)


(use-package web-mode
    :ensure t
    :mode "\\.html\\'"
    :mode "\\.json\\'"
    :mode "\\.js\\'")

(setq company-dabbrev-downcase nil)

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
    (find-file templateFile)
    )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(cursor-type '(bar . 2))
 '(custom-safe-themes
   '("bf815eb0b3031589aa53b6e01c57fa31e6fd367286204d2c15b6c07173ac63dc" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "d916b686ba9f23a46ee9620c967f6039ca4ea0e682c1b9219450acee80e10e40" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(ensime-sem-high-faces
   '((var :foreground "#000000" :underline
      (:style wave :color "yellow"))
     (val :foreground "#000000")
     (varField :foreground "#600e7a" :slant italic)
     (valField :foreground "#600e7a" :slant italic)
     (functionCall :foreground "#000000" :slant italic)
     (implicitConversion :underline
      (:color "#c0c0c0"))
     (implicitParams :underline
      (:color "#c0c0c0"))
     (operator :foreground "#000080")
     (param :foreground "#000000")
     (class :foreground "#20999d")
     (trait :foreground "#20999d" :slant italic)
     (object :foreground "#5974ab" :slant italic)
     (package :foreground "#000000")
     (deprecated :strike-through "#000000")))
 '(exwm-floating-border-color "#2e2f37")
 '(fci-rule-color "#3E4451")
 '(highlight-indent-guides-method 'character)
 '(highlight-tail-colors ((("#2d3e3e" "#2d3e3e") . 0) (("#333d49" "#333d49") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#282a36" "#57c7ff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#282a36" "#5af78e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#282a36" "#848688"))
 '(lsp-eldoc-enable-hover nil)
 '(lsp-go-analyses '(("unusedparams" . t) ("assign" . t) ("unusedwrite" . t)))
 '(lsp-go-hover-kind "FullDocumentation")
 '(lsp-go-use-gofumpt t)
 '(lsp-ui-doc-max-height 30)
 '(lsp-ui-doc-max-width 60)
 '(objed-cursor-color "#ff5c57")
 '(package-selected-packages
   '(goto-line-preview goto-line-previw doom-modeline tabbar magit git-emacs git highlight-indent-guides highlight-indentation highlight-indents vs-light-theme intellij-theme flycheck-golangci-lint js3-mode poly-markdown xref-js2 js2-refactor js2-mode json-mode multi-web-mode lsp-python-ms protobuf-mode web-mode go-mode company flycheck lsp-ui lsp-mode doom-themes neotree all-the-icons-dired yasnippet-snippets yasnippet use-package))
 '(pdf-view-midnight-colors (cons "#f9f9f9" "#282a36"))
 '(rustic-ansi-faces
   ["#282a36" "#ff5c57" "#5af78e" "#f3f99d" "#57c7ff" "#ff6ac1" "#9aedfe" "#f9f9f9"])
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#5af78e")
    (cons 40 "#8df793")
    (cons 60 "#c0f898")
    (cons 80 "#f3f99d")
    (cons 100 "#f7e38c")
    (cons 120 "#fbcd7c")
    (cons 140 "#ffb86c")
    (cons 160 "#ff9e88")
    (cons 180 "#ff84a4")
    (cons 200 "#ff6ac1")
    (cons 220 "#ff659d")
    (cons 240 "#ff607a")
    (cons 260 "#ff5c57")
    (cons 280 "#e06663")
    (cons 300 "#c1716f")
    (cons 320 "#a27b7b")
    (cons 340 "#e2e4e5")
    (cons 360 "#e2e4e5")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:background "#333" :foreground "#fafafa" :weight normal))))
 '(tab-bar ((t (:background "#1e1e1e" :weight normal :width normal)))))
