(require 'package)

(setq package-archives '(("melpa-stb" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless
    (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)
    (setq use-package-always-ensure t))

(when window-system (set-frame-size (selected-frame) 110 32))
(setq warning-minimum-level
      :emergency)

(global-set-key (kbd "S-<up>") 'windmove-up)
(global-set-key (kbd "S-<down>") 'windmove-down)
(global-set-key (kbd "S-<left>") 'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)

(defun calc-region (point mark)
    (interactive "r")
    (setq result (calc-eval
                  (buffer-substring
                   point
                   mark)))
    (insert (concat " = " result)))

(use-package
    ibuffer-sidebar
    :ensure t
    :commands (ibuffer-sidebar-toggle-sidebar)
    :config (setq ibuffer-sidebar-use-custom-font t)
    ;;(setq ibuffer-sidebar-face `(:family "Helvetica" :height 140))
    )

(use-package
    restclient
    :ensure t)

(use-package
    mark-multiple
    :ensure t)

(use-package
    multiple-cursors
    :ensure t)

(global-set-key (kbd "C->") 'mark-next-like-this)

(use-package
    mwim
    :ensure t
    :config (global-set-key (kbd "M-a") 'mwim-beginning)
    (global-set-key (kbd "M-e") 'mwim-end))

(use-package
    dumb-jump
    :ensure t)

;; to make it work install
;; apt-get install silversearcher-ag
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(use-package
    bm
    :ensure t
    :demand t
    :init
    ;; restore on load (even before you require bm)
    (setq bm-restore-repository-on-load t)

    ;; remove bookmark after jump
    ;; (setq temporary-bookmark-p t)
    :config
    ;; Allow cross-buffer 'next'
    (setq bm-cycle-all-buffers t)

    ;; where to store persistant files
    (setq bm-repository-file "~/.emacs.d/bm-repository")

    ;; save bookmarks
    (setq-default bm-buffer-persistence t)

    ;; Loading the repository from file when on start up.
    (add-hook 'after-init-hook 'bm-repository-load)

    ;; aving bookmarks
    (add-hook 'kill-buffer-hook #'bm-buffer-save)

    ;; Saving the repository to file when on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook #'(lambda nil (bm-buffer-save-all)
                                     (bm-repository-save)))

    ;; The `after-save-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state.
    (add-hook 'after-save-hook #'bm-buffer-save)

    ;; Restoring bookmarks
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)

    ;; The `after-revert-hook' is not necessary to use to achieve persistence,
    ;; but it makes the bookmark data in repository more in sync with the file
    ;; state. This hook might cause trouble when using packages
    ;; that automatically reverts the buffer (like vc after a check-in).
    ;; This can easily be avoided if the package provides a hook that is
    ;; called before the buffer is reverted (like `vc-before-checkin-hook').
    ;; Then new bookmarks can be saved before the buffer is reverted.
    ;; Make sure bookmarks is saved before check-in (and revert-buffer)
    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

    :bind (("M-1" . bm-toggle)
           ("M-2" . bm-previous)
           ("M-3" . bm-next)))

(add-hook 'term-mode-hook (lambda ()
                              (term-set-escape-char ?\C-`)))

(use-package
    webpaste
    :ensure t
    :bind (("C-c C-p C-r" . webpaste-paste-region))
    :config (progn
                (setq webpaste-provider-priority '("dpaste.org" "ix.io"))))

(use-package
    yasnippet
    :ensure t
    :commands yas-minor-mode
    :hook (go-mode . yas-minor-mode))

(use-package
    highlight-indent-guides
    :ensure t)

(use-package
    ace-jump-mode
    :ensure t)

(global-set-key (kbd "M-m") 'ace-jump-char-mode)
(global-set-key (kbd "M-o") 'next-window-any-frame)

(use-package
    ace-window
    :ensure t)

(use-package
    block-nav
    :ensure t)

(use-package
    selected
    :ensure t
    :commands selected-minor-mode
    :bind (:map selected-keymap
                ("r" . calc-region)
                ("q" . selected-off)
                ("c" . webpaste-paste-region)
                ("u" . upcase-region)
                ("d" . downcase-region)
                ("w" . count-words-region)
                ("m" . apply-macro-to-region-lines)))

(selected-global-mode 1)

;;(use-package magit
;;    :ensure t)

;;(defun my-highlighter (level responsive display)
;;    (if (> 1 level)
;;        nil
;;        (highlight-indent-guides--highlighter-default level responsive display)))

;;(setq highlight-indent-guides-highlighter-function 'my-highlighter)

;;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package
    yasnippet-snippets
    :ensure t)

(use-package
    all-the-icons
    :ensure t
    :init
    ;;(all-the-icons-install-fonts)
    )

(use-package
    goto-line-preview
    :ensure t)

(use-package
    doom-modeline
    :ensure t
    :init (doom-modeline-mode 1))
(setq doom-modeline-minor-modes nil)

(use-package
    all-the-icons-dired
    :ensure t)

(use-package
    neotree
    :ensure t)

(setq custom-safe-themes t)

(use-package
    doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (setq doom-themes-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    )

;;(load-theme 'github-modern t)

;; Global settings (defaults)
(setq doom-themes-enable-bold t ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;;(use-package vscode-dark-plus-theme
;;    :ensure t)

;; (use-package vscode-light-plus-theme
;;     :load-path "~/.emacs.d/elpa/vscode-light-plus-theme/"
;;     :ensure t)

;;(load-theme 'vscode-dark-plus t)

(use-package
    lsp-mode
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred)
    :hook (python-mode . lsp-deferred)
    :hook (dart-mode . lsp)
    :config (setq lsp-prefer-flymake nil)
    (setq lsp-flycheck-enable t)
    (setq lsp-headerline-breadcrumb-enable nil))

(use-package
    lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :config
    ;; (setq lsp-ui-doc-alignment 'window)
    (setq lsp-ui-doc-border "#dfdfdf")
    ;; (setq lsp-ui-doc-header nil)
    ;; (setq lsp-ui-doc-max-height 30)
    ;; (setq lsp-ui-doc-max-width 70)
    ;; (setq lsp-ui-doc-position 'top)
    ;; (setq lsp-ui-doc-use-webkit nil)
    (set-face-attribute 'lsp-ui-doc-background nil
                        :background "#f9f9f9"))

(use-package
    elisp-format
    :ensure t)

(use-package
    rainbow-mode
    :ensure t
    :init
    (rainbow-mode 1))


(use-package
    flycheck-golangci-lint
    :ensure t)

;;(set-face-attribute 'region nil :background "#ccc" :foreground "#ffffFF")

(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

(show-paren-mode 1)
;;(set-face-background 'show-paren-match "#dfdfdf")
;;(set-face-foreground 'show-paren-match "black")
;;(set-face-attribute 'show-paren-match nil :weight 'bold)


(global-hl-line-mode 1) ;; Подсвечивать текущую строку
;;(set-face-background 'hl-line "#dfdfdf")
;;(set-face-foreground 'highlight nil)


(global-display-line-numbers-mode)
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)
(setq echo-keystrokes 0.1 use-dialog-box nil visible-bell t)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-j") 'newline-and-indent)

(global-set-key (kbd "C-<up>") 'scroll-down-line)
(global-set-key (kbd "C-<down>") 'scroll-up-line)
(global-set-key (kbd "C-<right>") 'flycheck-next-error)
(global-set-key (kbd "C-<left>") 'flycheck-previous-error)

(global-set-key (kbd "C-<tab>") 'buffer-menu)
;; (global-set-key (kbd "C-<tab>") 'switch-to-next-buffer)
;; (global-set-key (kbd "C-<tab> backspace") 'switch-to-prev-buffer)

(defvar --backup-directory "~/.emacs.d/backups")
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t ; backup of a file the first time it is saved.
      backup-by-copying t ; don't clobber symlinks
      version-control t   ; version numbers for backup files
      delete-old-versions t      ; delete excess backup files silently
      delete-by-moving-to-trash t kept-old-versions 6 ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9 ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t ; auto-save every buffer that visits a file
      auto-save-timeout 20 ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
      )

(defun backups()
    (interactive)
    (find-file "~/.emacs.d/backups"))

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
(setq show-paren-style 'parenthesis)
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

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                  ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(add-hook 'yaml-mode-hook (lambda ()
                              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(add-hook 'dired-mode-hook (lambda()
                               (define-key dired-mode-map "b" 'dired-up-directory)
                               (dired-hide-details-mode)))

(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries 'left)
(set-face-attribute 'default nil
                    :font "Monospace 11")
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

;;(global-set-key (kbd "C-x b") 'buffer-menu)
(global-set-key (kbd "C-x C-b") 'buffer-menu)

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

(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-k") 'custom-kill-line)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)
(global-set-key (kbd "M-d") 'forward-delete-word)
(global-set-key (kbd "M-p") 'backward-page)
(global-set-key (kbd "M-n") 'forward-page)
;;(global-set-key (kbd "C-M-p") 'upBlock)
;;(global-set-key (kbd "C-M-n") 'downBlock)

(defun backwardParagraph()
    (interactive)
    (previous-line)
    (previous-line)
    (previous-line)
    (previous-line)
    (previous-line)
    (previous-line)
    (previous-line)
    )


(defun forwardParagraph()
    (interactive)
    (next-line)
    (next-line)
    (next-line)
    (next-line)
    (next-line)
    (next-line)
    (next-line)
    )

(global-set-key (kbd "C-M-p") 'backwardParagraph)
(global-set-key (kbd "C-M-n") 'forwardParagraph)
(global-set-key (kbd "C-M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j") 'lsp-execute-code-action)
(global-set-key (kbd "C-x C-g") 'goto-line-preview)
(global-set-key (kbd "C-r") 'replace-string)

(use-package
    flycheck
    :ensure t)

(use-package
    company
    :init (setq company-idle-delay t)
    (setq company-tooltip-align-annotations t)
    (setq company-dabbrev-char-regexp "[A-z:-]")
    :hook (after-init . global-company-mode)
    :bind (:map prog-mode-map
                ("C-i" . company-indent-or-complete-common)
                ("C-M-i" . counsel-company)))

(use-package
    go-mode
    :ensure t
    :config (setq lsp-go-hover-kind "FullDocumentation")
    (setq lsp-go-use-gofumpt t)
    (setq lsp-go-analyses '(("nilness" . t)
                            ("unusedparams" . t)
                            ("assign" . t)
                            ("unusedwrite" . t))))

(defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'flycheck-mode)

(add-hook 'before-save-hook 'fmt)

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

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package
    company
    :ensure t
    :config
    ;; Optionally enable completion-as-you-type behavior.
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1))

(use-package
    multi-web-mode

    :ensure
    :mode "\\.vue\\'"
    :config (setq mweb-default-major-mode 'web-mode)
    (setq mweb-tags '((css-mode "<style.*>" "</style>")))
    (setq mweb-filename-extensions '("vue" "html")))

(multi-web-global-mode 1)


(use-package
    web-mode
    :ensure t
    :mode "\\.html\\'"
    :mode "\\.json\\'"
    :mode "\\.js\\'")

(setq company-dabbrev-downcase nil)

(use-package
    protobuf-mode
    :ensure t)

(font-lock-add-keywords
 'go-mode
 '(("\\<\\(\\|int\\|string\\|bool\\|byte\\|complex\\|complex64\\|complex128\\|uint16\\|false\\|float32\\|float64\\|int\\|int8\\|int16\\|uint32\\|int32\\|int64\\|iota\\|uint64\\|true\\|uint\\|uint8\\|uintptr\\)\\>"
    . font-lock-type-face)))


(defun reverse-input-method (input-method)
    "Build the reverse mapping of single letters from INPUT-METHOD."
    (interactive (list (read-input-method-name "Use input method (default current): ")))
    (if (and input-method
             (symbolp input-method))
        (setq input-method (symbol-name input-method)))
    (let ((current current-input-method)
          (modifiers '(nil (control)
                       (meta)
                       (control meta))))
        (when input-method (activate-input-method input-method))
        (when (and current-input-method
                   quail-keyboard-layout)
            (dolist (map (cdr (quail-map)))
                (let* ((to (car map))
                       (from (quail-get-translation (cadr map)
                                                    (char-to-string to) 1)))
                    (when (and (characterp from)
                               (characterp to))
                        (dolist (mod modifiers)
                            (define-key local-function-key-map (vector (append mod (list from)))
                                (vector (append mod (list to)))))))))
        (when input-method (activate-input-method current))))

(reverse-input-method 'russian-computer)

(defun dir ()
    (interactive)
    (find-file default-directory))

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
    (find-file templateFile))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#191919" "#FF5E5E" "#468800" "#E9FDAC" "#8CDAFF" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(cursor-type '(bar . 2))
 '(custom-safe-themes
   '("1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "f4c8f0b999a6407211a899401315a628e1a5ae2f408c04a33b14d7aa3ed86187" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" default))
 '(display-time-day-and-date nil)
 '(exwm-floating-border-color "#121212")
 '(fci-rule-color "#515151")
 '(highlight-tail-colors ((("#1d2416" "#1d2416") . 0) (("#232c30" "#202c30") . 20)))
 '(ibuffer-sidebar-refresh-timer 0)
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#FFFFFF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#468800"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(linum-format 'dynamic)
 '(lsp-eldoc-enable-hover nil)
 '(nil nil t)
 '(objed-cursor-color "#FF5E5E")
 '(package-selected-packages
   '(ace-window rainbow-mode elisp-format vscdark-theme yasnippet-snippets yaml-mode wttrin webpaste web-mode vue-mode vscode-dark-plus-theme vs-light-theme use-package tao-theme tango-plus-theme tabbar spacemacs-theme smartparens selected restclient protobuf-mode projectile nord-theme neotree mwim multiple-cursors multi-web-mode mark-multiple magit lsp-ui lsp-python-ms light-soap-theme json-reformat jetbrains-darcula-theme intellij-theme ibuffer-sidebar highlight-indentation highlight-indent-guides goto-line-preview google-translate google go-mode github-theme github-modern-theme git format-all flycheck-golangci-lint flatui-theme expand-region espresso-theme epc dumb-jump doom-themes doom-modeline dashboard company centaur-tabs bm block-nav avy atom-one-dark-theme all-the-icons-dired afternoon-theme ace-jump-mode))
 '(pdf-view-midnight-colors (cons "#d4d4d4" "#191919"))
 '(rustic-ansi-faces
   ["#191919" "#FF5E5E" "#468800" "#E9FDAC" "#8CDAFF" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(vc-annotate-background "#191919")
 '(vc-annotate-color-map
   (list
    (cons 20 "#468800")
    (cons 40 "#7caf39")
    (cons 60 "#b2d672")
    (cons 80 "#E9FDAC")
    (cons 100 "#efd98e")
    (cons 120 "#f5b671")
    (cons 140 "#FC9354")
    (cons 160 "#e98e78")
    (cons 180 "#d78a9c")
    (cons 200 "#C586C0")
    (cons 220 "#d8789f")
    (cons 240 "#eb6b7e")
    (cons 260 "#FF5E5E")
    (cons 280 "#dd6464")
    (cons 300 "#bb6a6b")
    (cons 320 "#997071")
    (cons 340 "#515151")
    (cons 360 "#515151")))
 '(vc-annotate-very-old-color nil))

;;(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
;; '(lsp-ui-doc-background ((t (:background "#f9f9f9")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
