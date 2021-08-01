(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	    ("melpa-stb" . "https://stable.melpa.org/packages/")
	    ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)
    (setq use-package-always-ensure t))

(when window-system (set-frame-size (selected-frame) 110 32))
(setq warning-minimum-level :emergency)

(unless (require 'all-the-icons nil t)
    (use-package all-the-icons
        :ensure t)
    (all-the-icons-install-fonts))

(use-package all-the-icons
    :ensure t)

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
    (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    (doom-themes-treemacs-config)
    )

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(load-theme 'doom-snazzy t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

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

(use-package lsp-mode
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred)
    :config (progn
                ;; use flycheck, not flymake
                (setq lsp-prefer-flymake nil)))

;;;;(use-package company :ensure t) ;; Auto-complete

(use-package lsp-mode
    :hook (dart-mode . lsp))

;; Optional Flutter packages
(use-package hover :ensure t) ;; run app from desktop without emulator

;; Optional - provides fancier overlays.
(use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode)

(use-package go-mode
    :ensure t)

(defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'flycheck-mode)


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

;; Optional - provides snippet support.
(use-package yasnippet
    :ensure t
    :commands yas-minor-mode
    :hook (go-mode . yas-minor-mode))

(use-package web-mode
    :ensure t
    :mode "\\.html\\'"
    :mode "\\.js\\'"
    :mode "\\.vue\\'"
    :mode "\\.json\\'")

(use-package protobuf-mode
    :ensure t)

(font-lock-add-keywords 'go-mode      '(("\\<\\(\\|int\\|string\\|bool\\|byte\\|complex\\|complex64\\|complex128\\|uint16\\|false\\|float32\\|float64\\|int\\|int8\\|int16\\|uint32\\|int32\\|int64\\|iota\\|uint64\\|true\\|uint\\|uint8\\|uintptr\\)\\>" . font-lock-type-face)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "8feca8afd3492985094597385f6a36d1f62298d289827aaa0d8a62fe6889b33c" "d916b686ba9f23a46ee9620c967f6039ca4ea0e682c1b9219450acee80e10e40" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "7546a14373f1f2da6896830e7a73674ef274b3da313f8a2c4a79842e8a93953e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "5379937b99998e0510bd37ae072c7f57e26da7a11e9fb7bced8b94ccc766c804" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "a3bdcbd7c991abd07e48ad32f71e6219d55694056c0c15b4144f370175273d16" "0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "95d0ed21bb0e919be7687a25ad59a1c2c8df78cbe98c9e369d44e65bfd65b167" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "ff3c57a5049010a76de8949ddb629d29e2ced42b06098e046def291989a4104a" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "f08d2081f6783a5712cdce418f3962bd97a2054e8960609aad53f013a8b6f1cc" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "1d78d6d05d98ad5b95205670fe6022d15dabf8d131fe087752cc55df03d88595" "f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" default)))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#000000" :underline
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
     (deprecated :strike-through "#000000"))))
 '(fci-rule-color "#505050")
 '(jdee-db-active-breakpoint-face-colors (cons "#1b1d1e" "#fc20bb"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1b1d1e" "#60aa00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1b1d1e" "#505050"))
 '(minimap-mode t)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(nrepl-message-colors
   (quote
    ("#183691" "#969896" "#a71d5d" "#969896" "#0086b3" "#795da3" "#a71d5d" "#969896")))
 '(objed-cursor-color "#d02b61")
 '(package-selected-packages
   (quote
    (neotree tabbar powerline all-the-icons-dired jetbrains github-modern-theme dockerfile-mode dart-mode intellij-theme vs-light-theme github-theme vue-mode js2-mode cobalt yaml-mode sql-indent flycheck lsp-ui web-mode yasnippet lsp-mode go-mode company doom-themes use-package)))
 '(pdf-view-midnight-colors (cons "#dddddd" "#1b1d1e"))
 '(rustic-ansi-faces
   ["#1b1d1e" "#d02b61" "#60aa00" "#d08928" "#6c9ef8" "#b77fdb" "#00aa80" "#dddddd"])
 '(vc-annotate-background "#1b1d1e")
 '(vc-annotate-color-map
   (list
    (cons 20 "#60aa00")
    (cons 40 "#859f0d")
    (cons 60 "#aa931a")
    (cons 80 "#d08928")
    (cons 100 "#d38732")
    (cons 120 "#d6863d")
    (cons 140 "#da8548")
    (cons 160 "#ce8379")
    (cons 180 "#c281aa")
    (cons 200 "#b77fdb")
    (cons 220 "#bf63b2")
    (cons 240 "#c74789")
    (cons 260 "#d02b61")
    (cons 280 "#b0345c")
    (cons 300 "#903d58")
    (cons 320 "#704654")
    (cons 340 "#505050")
    (cons 360 "#505050")))
 '(vc-annotate-very-old-color nil))

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
    (find-file "~/")
    )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
