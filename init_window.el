(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(set-frame-size (selected-frame) 120 33)

;; doom-badger
;; doom-monokai-spectrum
;; doom-ir-black
;;
;; doom-dark+ is good enought
;;

;; spead-up lsp-mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024 50)) ;; 50mb
(setq lsp-log-io nil)
(setq lsp-file-watch-threshold 2000)
(setq lsp-enable-file-watchers nil)

;;(setq lsp-idle-delay 0.500)

(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

(use-package smart-shift
 	:ensure t)

(use-package jq-format
	:ensure t
	:after json-mode)

(use-package expand-region
	:ensure t)

(use-package yaml-mode
	:ensure t
	:config
	(setq-default yaml-indent-offset 4))

(defun lsp-yaml-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t))

(add-hook 'yaml-mode-hook #'lsp-yaml-install-save-hooks)
(add-hook 'yaml-mode-hook #'lsp)
(add-hook 'yaml-mode-hook #'flycheck-mode)

(unless
    (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)
    (setq use-package-always-ensure t))

(setq warning-minimum-level :emergency)

(load "~/.emacs.d/custom.el")
(load "~/.emacs.d/remaps.el")
(load "~/.emacs.d/keybindings.el")

(use-package
    mu
    :ensure t)

(use-package
    no-littering
    :ensure t
    :config
    (setq auto-save-file-name-transforms
          `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package
    magit
    :ensure t
	:config
	(setq magit-ediff-dwim-show-on-hunks t))

(use-package
    vdiff
    :ensure t)

(use-package
    vdiff-magit
    :ensure t)

(use-package
    helm
    :ensure t)

(use-package
    edit-indirect
    :ensure t)

(use-package
    graphql-mode
    :ensure t
    ;;:defer
    :mode "\\.graphqls\\'")

(use-package
    html-to-markdown
    :ensure t
    ;;:defer
    )

(add-hook 'before-save-hook #'md)

(use-package
    markdown-mode
    :ensure t
    ;;:defer
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

(use-package
    key-chord
    :ensure t
    :config
	;; (key-chord-define-global "<<" 'smart-shift-left)
	;; (key-chord-define-global ">>" 'smart-shift-right)
    )

(key-chord-mode 1)

(use-package
    typescript-mode
    :ensure t
    ;;:defer
    )

(use-package
    restclient
    :ensure t
    ;;:defer
    )

(use-package
    dockerfile-mode
    :ensure t
    ;;:defer
    )

(use-package
    mark-multiple
    :ensure t
    ;;:defer
    )

(use-package
    multiple-cursors
    :ensure t
    ;;:defer
    )

(use-package
    bm
    :ensure t
    ;;:defer
    :init

    (setq bm-restore-repository-on-load t)

    :config

    (setq bm-cycle-all-buffers t)

    (setq bm-repository-file "~/.emacs.d/bm-repository")

    (setq-default bm-buffer-persistence t)

    (add-hook 'after-init-hook 'bm-repository-load)

    (add-hook 'kill-buffer-hook #'bm-buffer-save)

    (add-hook 'kill-emacs-hook #'(lambda nil (bm-buffer-save-all)
                                     (bm-repository-save)))

    (add-hook 'after-save-hook #'bm-buffer-save)

    ;; Restoring bookmarks
    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)

    (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
    :bind (("M-1" . bm-toggle)
           ("M-2" . bm-previous)
           ("M-3" . bm-next)))

(add-hook 'term-mode-hook (lambda ()
                              (term-set-escape-char ?\C-`)))

(use-package
    webpaste
    :ensure t
    ;;:defer
    :bind (("C-c C-p C-r" . webpaste-paste-region))
    :config (progn
                (setq webpaste-provider-priority '("dpaste.org" "ix.io"))))

(use-package
    yasnippet
    :ensure t
    ;;:defer
    :commands yas-minor-mode
    :hook (go-mode . yas-minor-mode))

(use-package
    highlight-indent-guides
    :ensure t)

;; (defun my-highlighter (level responsive display)
;; 	(if (> 1 level)
;;         nil
;;         (highlight-indent-guides--highlighter-default level responsive display)))
;; (setq highlight-indent-guides-highlighter-function 'my-highlighter)
;; (setq-default highlight-indent-guides-method 'character)
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package
    ace-jump-mode
    :ensure t
    ;;:defer
    )



(use-package
    ace-window
    :ensure t
    ;;:defer
    )

(defun fuck()
	(interactive)
	(message "fuck"))

(use-package
    selected
    :ensure t
    ;;:defer
    :commands selected-minor-mode
    :bind (:map selected-keymap
				;; ("M-p" . smart-shift-up)
				;; ("M-n" . smart-shift-down)
				;; ("M-f" . smart-shift-right)
				;; ("M-b" . smart-shift-left)
                ("TAB" . custom/indent-region)
                ("r" . calc-region)
                ("q" . selected-off)
				("c" . webpaste-paste-region)
				("u" . upcase-region)
				("d" . downcase-region)
				("w" . count-words-region)
				("m" . apply-macro-to-region-lines)))

(selected-global-mode 1)

(use-package
    yasnippet-snippets
    :ensure t
    ;;:defer
    )

(unless (package-installed-p 'all-the-icons)
    (use-package
        all-the-icons
        :ensure t
        :init
        (all-the-icons-install-fonts)))

(use-package
    all-the-icons
    :ensure t)

(use-package all-the-icons-ibuffer
    :ensure t
    :init
    (all-the-icons-ibuffer-mode 1)
    :config
    ;; Whether display the icons.
    (setq all-the-icons-ibuffer-icon t)

    ;; Whether display the colorful icons.
    ;; It respects `all-the-icons-color-icons'.
    (setq all-the-icons-ibuffer-color-icon t)

    ;; The default icon size in ibuffer.
    (setq all-the-icons-ibuffer-icon-size 1.0)

    ;; The default vertical adjustment of the icon in ibuffer.
    (setq all-the-icons-ibuffer-icon-v-adjust 0.0)

    ;; Use human readable file size in ibuffer.
    (setq  all-the-icons-ibuffer-human-readable-size t)

    ;; A list of ways to display buffer lines with `all-the-icons'.
    ;; See `ibuffer-formats' for details.
    all-the-icons-ibuffer-formats

    ;; Slow Rendering
    ;; If you experience a slow down in performance when rendering multiple icons simultaneously,
    ;; you can try setting the following variable
    (setq inhibit-compacting-font-caches t)
    )

(use-package
    goto-line-preview
    :ensure t
    ;;:defer
    )

(use-package
    doom-modeline
    :ensure t
    ;;:defer
    :init (doom-modeline-mode 1))

(setq doom-modeline-minor-modes nil)

(use-package
    all-the-icons-dired
    :ensure t)

;; (use-package
;;     neotree
;;     :ensure t
;;     :config
;;     (neotree-show)
;;     (setq-default neo-show-hidden-files t)
;;     (setq-default neo-autorefresh t)
;;     (setq-default neo-mode-line-type 'none)
;;     (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;     )

(setq custom-safe-themes t)

(use-package
    doom-themes
    :ensure t
    ;;:defer
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
          doom-themes-enable-italic t) ;; if nil, italics is universally disabled

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    (doom-themes-neotree-config)
    (setq doom-themes-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    )

(use-package
    vscode-dark-plus-theme
    :ensure t
    ;;:defer
    )

;;(load-theme 'vscode-dark-plus t)
(load-theme 'doom-badger t)

;; Global settings (defaults)
(setq doom-themes-enable-bold t ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled


(setq lsp-sqls-connections
      '(
		((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5433 user=root password=password dbname=postgres sslmode=disable TimeZone='Europe/Moscow'"))
		))

;; (use-package
;;     eglot
;;     :ensure t)

;; (defun eglot-go-install-save-hooks ()
;;     (add-hook 'before-save-hook #'eglot-format-buffer t t)
;;     (add-hook 'before-save-hook #'eglot-code-action-organize-imports t t))

;; (add-hook 'go-mode-hook #'eglot-go-install-save-hooks)
;; (add-hook 'go-mode-hook 'eglot-ensure)

(use-package
    lsp-mode
    :ensure t

	:custom
	(lsp-vetur-format-default-formatter-css "none")
	(lsp-vetur-format-default-formatter-html "none")
	(lsp-vetur-format-default-formatter-js "none")
	(lsp-vetur-validation-template nil)

    :commands (lsp lsp-deferred)
    :hook (sql-mode . lsp-deferred)
    :hook (yaml-mode . lsp-deferred)
    :hook (dockerfile-mode . lsp-deferred)
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
    ;;(setq lsp-ui-doc-border "#dfdfdf")
    ;; (setq lsp-ui-doc-header nil)
    ;; (setq lsp-ui-doc-max-height 30)
    ;; (setq lsp-ui-doc-max-width 70)
    ;; (setq lsp-ui-doc-position 'top)
    ;; (setq lsp-ui-doc-use-webkit nil)
    ;;(set-face-attribute 'lsp-ui-doc-background nil
    ;;:background "#f9f9f9")
    )

(use-package
    elisp-format
    :ensure t)

(use-package
    rainbow-mode
    :ensure t)

(rainbow-mode 1)

;; (use-package
;;    flycheck-golangci-lint
;;    :ensure t
;; ;;:defer
;;)

;;(set-face-attribute 'region nil :background "#ccc" :foreground "#ffffFF")

;; (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

(show-paren-mode 1)
;;(set-face-background 'show-paren-match "#dfdfdf")
;;(set-face-foreground 'show-paren-match "black")
;;(set-face-attribute 'show-paren-match nil :weight 'bold)


(global-hl-line-mode 1) ;; Подсвечивать текущую строку
;;(set-face-background 'hl-line "#dfdfdf")
;;(set-face-foreground 'highlight nil)


(global-display-line-numbers-mode 1)
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)
(setq echo-keystrokes 0.1 use-dialog-box nil visible-bell t)

;; (defvar --backup-directory "~/.emacs.d/backups")

;; (if (not (file-exists-p --backup-directory))
;;     (make-directory --backup-directory t))
;; (setq backup-directory-alist `(("." . ,--backup-directory)))
;; (setq make-backup-files t ; backup of a file the first time it is saved.
;;       backup-by-copying t ; don't clobber symlinks
;;       version-control t   ; version numbers for backup files
;;       delete-old-versions t      ; delete excess backup files silently
;;       delete-by-moving-to-trash t kept-old-versions 6 ; oldest versions to keep when a new numbered backup is made (default: 2)
;;       kept-new-versions 9 ; newest versions to keep when a new numbered backup is made (default: 2)
;;       auto-save-default t ; auto-save every buffer that visits a file
;;       auto-save-timeout 20 ; number of seconds idle time before auto-save (default: 30)
;;       auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
;;       )

(add-hook 'prog-mode-hook #'hs-minor-mode)

(require 'imenu)
(setq imenu-auto-rescan      t)
(setq imenu-use-popup-menu nil)
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
(setq-default cursor-type 'box)
(set-cursor-color "green")
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

;;(add-hook 'yaml-mode-hook (lambda ()
;;                              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(add-hook 'dired-mode-hook (lambda()
                               (define-key dired-mode-map "b" 'dired-up-directory)
                               (dired-hide-details-mode)))

(setq-default indicate-empty-lines nil)

(setq-default indicate-buffer-boundaries 'left)
(set-face-attribute 'default nil :font "Monospace Bold 11")
(setq display-time-24hr-format t)
(display-time-mode             t)
(size-indication-mode          t)
(setq word-wrap          t)
(global-visual-line-mode t)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)
(setq lisp-indent-function  'common-lisp-indent-function)
(setq scroll-step               1)
(setq scroll-margin             5)
(setq scroll-conservatively 10000)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq x-select-enable-clipboard t)

(use-package
    flycheck
    :ensure t)

(use-package
    company
    :ensure t
    :init

    (setq company-idle-delay t)
    (setq company-tooltip-align-annotations t)
    (setq company-dabbrev-char-regexp "[A-z:-]")

    :hook (after-init . global-company-mode)
    :bind (:map prog-mode-map
                ("C-i" . company-indent-or-complete-common)
                ("C-M-i" . counsel-company)))

(use-package
    go-mode
    :ensure t
    :config
    (setq lsp-go-hover-kind "FullDocumentation")
    (setq lsp-go-use-gofumpt t)
    ;;(setq lsp-go-analyses '(
    ;;                         ("nilness" . t)
    ;;                         ("unusedparams" . t)
    ;;                         ("assign" . t)
    ;;                         ("unusedwrite" . t)))
	)
(defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp)
(add-hook 'go-mode-hook #'flycheck-mode)

(add-hook 'before-save-hook #'fmt)

;; (use-package
;;     multi-web-mode
;;     :ensure t
;;     ;;:defer
;;     :config (setq mweb-default-major-mode 'web-mode)
;;     (setq mweb-tags '((css-mode "<style.*>" "</style>")))
;;     (setq mweb-filename-extensions '("vue")))

;; (multi-web-global-mode 1)

(use-package
    web-mode
    :ensure t
    :mode "\\.vue\\'"
    :mode "\\.html\\'"
    :mode "\\.js\\'"
	:config
	(add-hook 'vue-mode-hook #'lsp))

(use-package
    json-mode
    :ensure t
    ;;:defer"
    :mode "\\.json\\'")

(add-hook 'json-mode-hook #'flycheck-mode)

(use-package
    protobuf-mode
    :ensure t
    ;;:defer
    )

(defconst my-protobuf-style
  '((indent-tabs-mode . nil)
	(c-basic-offset . 2)))

(add-hook 'protobuf-mode-hook
		  (lambda () (c-add-style "my-style" my-protobuf-style t)))

(font-lock-add-keywords
 'go-mode
 '(("\\<\\(\\|int\\|string\\|bool\\|byte\\|complex\\|complex64\\|complex128\\|uint16\\|false\\|float32\\|float64\\|int\\|int8\\|int16\\|uint32\\|int32\\|int64\\|iota\\|uint64\\|true\\|uint\\|uint8\\|uintptr\\)\\>"
	. font-lock-type-face)))

(font-lock-add-keywords
 'sql-mode
 '(("\\<\\(\\|int\\|string\\|\\|TINYINT\\|complex\\|complex64\\|complex128\\|uint16\\|false\\|float32\\|float64\\|int\\|int8\\|int16\\|uint32\\|int32\\|int64\\|iota\\|uint64\\|datetime\\|bigint\\|\\|uintptr\\)\\>"
	. font-lock-type-face)))

(font-lock-add-keywords
 'sql-mode
 '(("\\<\\(\\|\\|string\\|UNSIGNED\\|\\|if\\|complex64\\|use\\|uint16\\|false\\|float32\\|float64\\|\\|int8\\|int16\\|uint32\\|int32\\|int64\\|iota\\|uint64\\|\\|\\|AUTO_INCREMENT\\|uintptr\\)\\>"
	. font-lock-keyword-face)))

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

(require 'subr-x)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#191919" "#FF5E5E" "#468800" "#E9FDAC" "#8CDAFF" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(custom-safe-themes
   '("cf861f5603b7d22cb3545a7c63b2ee424c34d8ed3b3aa52d13abfea4765cffe7" "2f1ed0c737c498295c7b72396cb9c168556b7bfc637967ec2fba044b63d55ece" "9c45a0351e89fe6e950ed7ff7c7bf785344dcb56598e6d97532c638c90000309" "665fbb8e59d19f13bb1f1215af8180758b464eefe239b9a59ccd57486958b68b" "62eca1d5615ff9695691704030914c470e6a6c0244d5049893715bcbd202c0dd" "8543bb312515c9e477f629a729c2bf8a4f71332c5756f32f3cb5fbbd0a0a785a" "303cfaa6ce6653d3299583f9f002107487860b701d314fff589b7df77263d5fd" "98ef70f6028d10db0a3e1178b741df39c2dfefad97eda6ff1f17d5e83fe32179" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "f4c8f0b999a6407211a899401315a628e1a5ae2f408c04a33b14d7aa3ed86187" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" default))
 '(display-time-day-and-date nil)
 '(exwm-floating-border-color "#121212")
 '(fci-rule-color "#515151")
 '(highlight-tail-colors ((("#1d2416" "#1d2416") . 0) (("#232c30" "#202c30") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#FFFFFF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#468800"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(linum-format 'dynamic)
 '(lsp-eldoc-enable-hover nil)
 '(nil nil t)
 '(objed-cursor-color "#FF5E5E")
 '(package-selected-packages
   '(graphql-mode dap-mode go-dlv indent-tools sql-sqlline sqlup-mode sql-smie format-sql reformatter sql-indent sqlformat html-to-markdown typescript-mode dockerfile-mode key-chord ace-window rainbow-mode elisp-format vscdark-theme yasnippet-snippets yaml-mode wttrin webpaste web-mode vue-mode vscode-dark-plus-theme vs-light-theme use-package tao-theme tango-plus-theme tabbar spacemacs-theme smartparens selected restclient protobuf-mode projectile nord-theme neotree mwim multiple-cursors multi-web-mode mark-multiple magit lsp-ui lsp-python-ms light-soap-theme json-reformat jetbrains-darcula-theme intellij-theme highlight-indentation highlight-indent-guides goto-line-preview google-translate google go-mode github-theme github-modern-theme git format-all flycheck-golangci-lint flatui-theme expand-region espresso-theme epc dumb-jump doom-themes doom-modeline dashboard company centaur-tabs bm block-nav avy atom-one-dark-theme all-the-icons-dired afternoon-theme ace-jump-mode))
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
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
