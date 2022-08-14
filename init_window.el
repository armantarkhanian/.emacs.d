;; Comment from github (to pull from magit)
(unless
    (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)
    (setq use-package-always-ensure t))

(setq warning-minimum-level :emergency)

(add-to-list 'auto-mode-alist '("\\org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(use-package nginx-mode :ensure t)

(global-so-long-mode 1)

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; spead-up lsp-mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024 50)) ;; 50mb
(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 1)
(setq create-lockfiles nil)
(setq lsp-log-io nil)
(setq lsp-file-watch-threshold 2000)
(setq lsp-enable-file-watchers nil)

(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

(use-package blamer
	:ensure t
	:bind (("s-i" . blamer-show-commit-info))
	:defer 20
	:custom
	(blamer-idle-time 0.3)
	(blamer-min-offset 70)
	:custom-face
	(blamer-face ((t :foreground "#7a88cf"
					 :background nil
					 :italic t)))
	:config
	;; Включим blamer по-умолчанию
	;;(global-blamer-mode 1)
	)

(use-package ujelly-theme
	:ensure t)

(use-package dakrone-theme
	:ensure t)

(use-package jenkinsfile-mode
	:ensure t)

(use-package jenkins
	:ensure t)

(use-package jenkins-watch
	:ensure t)

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
	(setq-default yaml-indent-offset 2))

;; (defun lsp-yaml-install-save-hooks ()
;; 	(add-hook 'before-save-hook #'lsp-format-buffer t t))

;; (add-hook 'yaml-mode-hook #'lsp-yaml-install-save-hooks)
;; (add-hook 'yaml-mode-hook #'lsp)
;; (add-hook 'yaml-mode-hook #'flycheck-mode)

(use-package
	no-littering
	:ensure t
	:config
	(setq auto-save-file-name-transforms
		  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package magit
	:ensure t)

;; To use magit-delta install "delta" binary into $PATH from https://dandavison.github.io/delta/installation.html
(use-package magit-delta
	:ensure t
	:hook (magit-mode . magit-delta-mode)
	:config
	(setq magit-delta-default-dark-theme "Visual Studio Dark+")
	(setq magit-delta-default-light-theme "Github")
	(setq magit-delta-hide-plus-minus-markers t)

	(setq magit-delta-delta-args '(
								   "--max-line-distance" "0.6"
								   "--24-bit-color" "always"
								   "--color-only"
								   ;; "--diff-highlight"
								   "--relative-paths"

								   ;; Colors
								   "--minus-style" "syntax #4b1818" ;; #4b1818 #171717
								   "--minus-emph-style" "syntax #6f1313"
								   "--plus-style"      "syntax #373d29" ;; #373d29 #171717
								   "--plus-emph-style" "syntax #4b5632"
								   ;; new comment
								   ))
	)

(with-eval-after-load 'magit-delta
	(set-face-attribute 'magit-diff-added-highlight nil
						:extend t)
	(set-face-attribute 'magit-diff-added nil
						:extend t)
	(set-face-attribute 'magit-diff-removed-highlight nil
						:extend t)
	(set-face-attribute 'magit-diff-removed nil
						:extend t))

(add-hook 'magit-delta-mode-hook
		  (lambda ()
			  (setq face-remapping-alist
					(seq-difference face-remapping-alist
									'((magit-diff-removed . default)
									  (magit-diff-removed-highlight . default)
									  (magit-diff-added . default)
									  (magit-diff-added-highlight . default))))))

(use-package
	helm
	:ensure t)

(use-package
	edit-indirect
	:ensure t)

(use-package
	graphql-mode
	:ensure t
	:mode "\\.graphqls\\'")

(use-package
	html-to-markdown
	:ensure t
	)

(add-hook 'before-save-hook #'md)

(use-package
	markdown-mode
	:ensure t
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
	:ensure t)

(use-package
	restclient
	:ensure t
	:mode (
		   ("\\rest\\'" . restclient-mode)
		   ("\\.rest\\'" . restclient-mode)
		   )
	)

(use-package
	dockerfile-mode
	:ensure t
	)

(use-package
	mark-multiple
	:ensure t
	)

(use-package
	multiple-cursors
	:ensure t
	)

(use-package
	bm
	:ensure t
	:init

	(setq bm-restore-repository-on-load t)

	:config
	(setq bm-highlight-style 'bm-highlight-only-line)

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
	:bind (("C-c C-p C-r" . webpaste-paste-region))
	:config (progn
				(setq webpaste-provider-priority '("dpaste.org" "ix.io"))))

(use-package
	highlight-indent-guides
	:ensure t)

;; (defun my-highlighter (level responsive display)
;;     (if (> 1 level)
;;         nil
;;         (highlight-indent-guides--highlighter-default level responsive display)))
;; (setq highlight-indent-guides-highlighter-function 'my-highlighter)
;; (setq-default highlight-indent-guides-method 'character)
;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package
	ace-jump-mode
	:ensure t
	)



(use-package
	ace-window
	:ensure t
	)

(defun fuck()
	(interactive)
	(message "fuck"))

(use-package
	selected
	:ensure t
	:commands selected-minor-mode
	:bind (:map selected-keymap
				;; ("M-p" . smart-shift-up)
				;; ("M-n" . smart-shift-down)
				;; ("M-f" . smart-shift-right)
				;; ("M-b" . smart-shift-left)
				("TAB" . custom/indent-region)
				("r" . calc-region)
				("к" . calc-region)
				("q" . selected-off)
				("c" . webpaste-paste-region)
				("t" . custom/titleize-region)
				("u" . upcase-region)
				("г" . upcase-region)
				("d" . downcase-region)
				("в" . downcase-region)
				("w" . count-words-region)
				("m" . apply-macro-to-region-lines)))

(selected-global-mode 1)

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
	;; all-the-icons-ibuffer-formats

	;; Slow Rendering
	;; If you experience a slow down in performance when rendering multiple icons simultaneously,
	;; you can try setting the following variable
	(setq inhibit-compacting-font-caches t)
	)

(use-package
	goto-line-preview
	:ensure t
	)

(use-package
	doom-modeline
	:ensure t
	:init (doom-modeline-mode 1)
	:config
	(setq doom-modeline-minor-modes nil)
	(setq doom-modeline-buffer-file-name-style 'buffer-name)
	(setq doom-modeline-enable-word-count nil)
	(setq doom-modeline-buffer-encoding t)
	(setq doom-modeline-indent-info nil)
	(setq doom-modeline-vcs-max-length 20)
	(setq doom-modeline-workspace-name nil)
	)

;; (setq-default header-line-format '("%e"
;; 								   (:eval
;; 									(doom-modeline-format--main))))

(setq-default mode-line-format '("%e"
								 (:eval
								  (doom-modeline-format--main))))

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
	:config
	;; Global settings (defaults)
	(setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
		  doom-themes-enable-italic t) ;; if nil, italics is universally disabled

	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config)
	(doom-themes-neotree-config)
	(setq doom-themes-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
	)

(use-package vscode-dark-plus-theme
	:ensure t)

(use-package github-theme
	:ensure t)

(use-package clues-theme
	:ensure t)

(use-package afternoon-theme
	:ensure t)

;; (load-theme 'clues t)
(load-theme 'vscode-dark-plus t)
;; (load-theme 'dakrone t)
;; (load-theme 'intellij t)
;; (load-theme 'doom-badger t)
;; (load-theme 'doom-opera t)
;; (load-theme 'afternoon t)
;; (load-theme 'doom-ephemeral t)
;; (load-theme 'doom-sourcerer t)
;; (load-theme 'solarized-wombat-dark t)
;; (load-theme 'ujelly t)
;; (load-theme 'dichromacy t)

;; Global settings (defaults)
(setq doom-themes-enable-bold t ; if nil, bold is universally disabled
	  doom-themes-enable-italic t) ; if nil, italics is universally disabled

(setq lsp-sqls-connections
	  '(
		((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5433 user=root password=password dbname=postgres sslmode=disable TimeZone='Europe/Moscow'"))
		))

(use-package ccls
	:ensure t
	:hook ((c-mode c++-mode objc-mode cuda-mode) .
		   (lambda () (require 'ccls) (lsp))))

(use-package
	lsp-java
	:ensure t)

(require 'lsp-java)

(use-package
	lsp-mode
	:ensure t
	:commands (lsp lsp-deferred)
	;;:hook (sql-mode . lsp-deferred)
	;;:hook (yaml-mode . lsp-deferred)
	;;:hook (dockerfile-mode . lsp-deferred)
	:hook (go-mode . lsp-deferred)
	:hook (rust-mode . lsp-deferred)
	:hook (kotlin-mode . lsp-deferred)
	;;:hook (c++-mode . lsp-deferred)
	:hook (java-mode . lsp)
	;;:hook (python-mode . lsp-deferred)
	;;:hook (dart-mode . lsp)
	:config
	(setq lsp-prefer-flymake :none)
	(setq lsp-flycheck-enable nil)
	(setq lsp-headerline-breadcrumb-enable nil))

;; (use-package sqlformat
;; 	:ensure t
;; 	:commands (sqlformat sqlformat-buffer sqlformat-region)
;; 	:hook (sql-mode . sqlformat-on-save-mode)
;; 	:init
;; 	(setq sqlformat-command 'pgformatter
;;           sqlformat-args '("-s2" "-g" "-u1")))

(use-package
	lsp-ui
	:ensure t
	:commands lsp-ui-mode
	:custom
	(lsp-ui-doc-enable t)
	(lsp-ui-doc-show-with-mouse nil)
	(lsp-ui-doc-show-with-cursor t)
	(lsp-ui-doc-alignment 'window)
	;; (lsp-ui-doc-border "#ccc")
	(lsp-ui-doc-header nil)
	(lsp-ui-doc-max-height 35)
	(lsp-ui-doc-max-width 80)
	(lsp-ui-doc-position 'top)
	(lsp-ui-doc-use-webkit nil)
	;; (set-face-attribute 'lsp-ui-doc-background nil :background "#eeeeee")
	)

(use-package
	elisp-format
	:ensure t)

(use-package rainbow-mode
	:ensure t
	:config
	(rainbow-mode 1))

;;(set-face-attribute 'region nil :background "#ccc" :foreground "#ffffFF")

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
(set-face-attribute 'default nil :font "Monospace 11")
(setq display-time-24hr-format t)
(display-time-mode             t)
(size-indication-mode          t)
(setq word-wrap          t)
(global-visual-line-mode t)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
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
	:ensure t
	:diminish flycheck-mode
	:config
	(global-flycheck-mode)
	;; disable jshint since we prefer eslint checking
	(setq-default flycheck-disabled-checkers
				  (append flycheck-disabled-checkers
						  '(javascript-jshint)))

	(setq flycheck-checkers '(javascript-eslint))
	;; use eslint with web-mode for jsx files
	(flycheck-add-mode 'javascript-eslint 'web-mode)
	(flycheck-add-mode 'javascript-eslint 'js2-mode)
	(flycheck-add-mode 'javascript-eslint 'js-mode)
	;; disable json-jsonlist checking for json files
	(setq-default flycheck-disabled-checkers
				  (append flycheck-disabled-checkers
						  '(json-jsonlist)))
	)

(use-package flycheck-golangci-lint
	:ensure t
	:custom
	(flycheck-golangci-lint-disable-all t)
	(flycheck-golangci-lint-enable-linters '(
											 "gochecknoglobals"
											 "gochecknoinits"
											 "revive"
											 "nakedret"
											 "gocyclo"
											 "gosimple"
											 "gofumpt"
											 "goconst"
											 "misspell"
											 "unconvert"
											 "varcheck"
											 "unused"
											 "deadcode"
											 "unparam"
											 "ineffassign"
											 "gocritic"
											 "prealloc"
											 "exportloopref"
											 "staticcheck"
											 "govet"
											 "errcheck"
											 "wsl"
											 ))
	)

(defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around '+flycheck-checker-get)

(use-package yasnippet
	:ensure t
	:config
	(yas-global-mode 1))

(use-package yasnippet-snippets
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
	flycheck-golangci-lint
	:ensure t)

;; (use-package lsp-javacomp
;; 	:ensure t)

;; :init
;; (add-hook 'java-mode-hook
;;           (lambda ()
;; 			  ;; Load lsp-javacomp before enabling lsp, so that javacomp client
;; 			  ;; is registed.
;; 			  (require 'lsp-javacomp')
;; 			  (lsp)
;; 			  ;; Use company-lsp as the company completion backend
;; 			  (set (make-variable-buffer-local 'company-backends) '(company-lsp))
;; 			  ;; Optional company-mode settings
;; 			  (set (make-variable-buffer-local 'company-idle-delay) 0.1)
;; 			  (set (make-variable-buffer-local 'company-minimum-prefix-length) 1)))
;; ;; Optional, make sure JavaComp is installed. See below.
;; :config
;; (lsp-javacomp-install-server))

(use-package
	kotlin-mode
	:ensure t)

(use-package
	rust-mode
	:ensure t)

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))

(setq rust-format-on-save t)


(use-package
	go-mode
	:ensure t
	:custom
	(lsp-go-use-gofumpt t)

	(lsp-go-analyses '(
					   (shadow . t)
					   ;; (fieldalignment . t)
					   (nilness . t)
					   (unusedparams . t)
					   (unusedwrite . t)
					   (unindent . t)
					   ))
	)

(defun lsp-go-install-save-hooks ()
	(add-hook 'before-save-hook #'lsp-format-buffer t t)
	(add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'before-save-hook #'fmt)

(add-hook 'go-mode-hook (lambda ()
							(lsp)
							(lsp-go-install-save-hooks)
							(flycheck-mode)
							(flycheck-golangci-lint-setup)
							(flycheck-select-checker 'golangci-lint)
							))

(add-hook 'go-mode-hook (lambda()
                            (flycheck-golangci-lint-setup)
                            (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))

(use-package
	web-mode
	:ensure t
	:mode "\\.html\\'"
	:mode "\\.vue\\'"
	:mode "\\.js\\'"
	:mode "\\.json\\'"
	:mode "\\.jsx\\'"
	:mode "\\.ts\\'"
	:mode "\\.tsx\\'"
	:config
	(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
	(setq web-mode-content-types-alist '(("tsx" . "\\.ts[x]?\\'")))
	(setq indent-tabs-mode nil)
	(setq web-mode-markup-indent-offset 2)
	(setq web-mode-code-indent-offset 2)
	(setq web-mode-css-indent-offset 2)
	(setq web-mode-attr-indent-offset 2)
	(setq web-mode-enable-auto-pairing t)
	(setq web-mode-enable-auto-closing t)


	(setq web-mode-style-padding 0)
	(setq web-mode-script-padding 0)
	(setq web-mode-script-padding 0)
	(setq web-mode-block-padding 0)

	(define-key web-mode-map (kbd "C--") 'web-mode-fold-or-unfold)
	(define-key web-mode-map (kbd "C-=") 'web-mode-fold-or-unfold)
	(setq web-mode-enable-current-element-highlight nil)
	)


(use-package
	protobuf-mode
	:ensure t)

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

(use-package git-gutter
	:ensure t
	:diminish
	;; :hook ((text-mode . git-gutter-mode)
	;; (prog-mode . git-gutter-mode))
	:config
	(setq git-gutter:update-interval 2)

	;; These characters are used in terminal mode
	(setq git-gutter:modified-sign "M")
	(setq git-gutter:added-sign "+")
	(setq git-gutter:deleted-sign "-")
	(set-face-foreground 'git-gutter:added "LightGreen")
	(set-face-foreground 'git-gutter:modified "LightGoldenrod")
	(set-face-foreground 'git-gutter:deleted "LightCoral"))
