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

(defun calc-region (point mark)
    (interactive "r")
    (setq result (calc-eval (buffer-substring point mark)))
    (insert (concat " = " result))
    )

(use-package restclient
    :ensure t)

(use-package mark-multiple
    :ensure t)

(use-package multiple-cursors
    :ensure t)

(global-set-key (kbd "C->") 'mark-next-like-this)

(use-package mwim
    :ensure t
    :config
    (global-set-key (kbd "M-a") 'mwim-beginning)
    (global-set-key (kbd "M-e") 'mwim-end)
    )

(use-package dumb-jump
    :ensure t)

;; to make it work install
;; apt-get install silversearcher-ag
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(use-package bm
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

    ;; Saving bookmarks
    (add-hook 'kill-buffer-hook #'bm-buffer-save)

    ;; Saving the repository to file when on exit.
    ;; kill-buffer-hook is not called when Emacs is killed, so we
    ;; must save all bookmarks first.
    (add-hook 'kill-emacs-hook #'(lambda nil
                                     (bm-buffer-save-all)
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


    :bind (
           ("M-1" . bm-toggle)
           ("M-2" . bm-previous)
           ("M-3" . bm-next))
    )

(use-package webpaste
    :ensure t
    :bind (
           ("C-c C-p C-r" . webpaste-paste-region)
           )
    :config
    (progn
        (setq webpaste-provider-priority '("dpaste.org" "ix.io"))))

(use-package yasnippet
    :ensure t
    :commands yas-minor-mode
    :hook (go-mode . yas-minor-mode))

(use-package highlight-indent-guides
    :ensure t)

(use-package ace-jump-mode
    :ensure t)

(use-package block-nav
    :ensure t
    :config
    )

(global-set-key (kbd "C-$") 'ace-jump-mode)

(use-package selected
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

(use-package vscode-dark-plus-theme
    :ensure t)

;; (use-package vscode-light-plus-theme
;;     :load-path "~/.emacs.d/elpa/vscode-light-plus-theme/"
;;     :ensure t)

(load-theme 'vscode-dark-plus t)

(use-package lsp-mode
    :ensure t
    :commands (lsp lsp-deferred)
    :hook (go-mode . lsp-deferred)
    :hook (python-mode . lsp-deferred)
    :hook (dart-mode . lsp)
    :config (progn
                (setq lsp-prefer-flymake nil)
                (setq lsp-flycheck-enable t))
    )

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
(set-face-foreground 'show-paren-match "white")
(set-face-attribute 'show-paren-match nil :weight 'bold)

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
(global-set-key (kbd "C-j") 'newline-and-indent)

(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)
(global-set-key (kbd "C-<right>") 'flycheck-next-error)
(global-set-key (kbd "C-<left>") 'flycheck-previous-error)
(global-set-key (kbd "C-<left>") 'flycheck-previous-error)

(global-set-key (kbd "C-<tab>") 'buffer-menu)
;; (global-set-key (kbd "C-<tab>") 'switch-to-next-buffer)
;; (global-set-key (kbd "C-<tab> backspace") 'switch-to-prev-buffer)

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
(set-face-attribute 'default nil :font "Monospace 11")
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

(use-package flycheck
    :ensure t)

(use-package company
    :init
    (setq company-idle-delay t)
	(setq company-tooltip-align-annotations t)
    (setq company-dabbrev-char-regexp "[A-z:-]")
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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(column-number-mode t)
 '(cursor-type '(bar . 2))
 '(custom-safe-themes
   '("5681d942c53f3dc2cf1b229318c3c72f81a271fce1937125ac39237a44151973" "349da127fde96292ac2020211a86f82917c3529ffc165e0749f9ccbb4a034e36" "9edf8f919cd749e363b2c090e7fdaa441d109c6b14d7413dbe6d30d50f320550" "05e1471a91228c4ff340f996e16ecade4b5027a473da8c747e7ee03bec05de99" "db8ce71267124e08c5d8b710a6628ca3476397367d579aba85456de62d570800" "53641db0fc56ba1d66c12b435f18d9f3569d8e2020c3e463a23e523a6d1586ab" "b61efaaa40a6bb681c0228aea90cca3db7bbf56070822be51086e097cf5a71cc" "ae3d18ed243d1782365a665d6a3af4e48bfd307bc8d7c54f7aa0ba2346db72b2" "f714cca172bfe1ca626e488a696d3af0defd044db9ecddb9055d3132fe861397" "9f9fe7c0a848c05d56200f99b79aff349a5810ee865cc273ec221db6b378e530" "0ce9f16a63c7017c90483cf3c877b5cb36c3c1528c29000e111837e058915469" "9e96d8878de13f975331c1755e224b9fa1dfbfeeee4b3e5ecf88584f9ece796e" "9b85ff1fdf060d2f3c5dfd1694e8314c643cc095530b963e8c241f452d6f4e90" "3c6be79921436833b081cae99368f50de55941313ba0267c0093fb4394956d8a" "8dc00551702c012c65654076fa0ce8b2228e90d5d9af7b5bb046db65a45b5587" "c8e4632e3ebff67bc2d47e6b36bd345bd2fb4c7a5fdd7c3dfd09a745702c2bdb" "9de09bf9269f2dd08fefb620bf26c9aa8dc664e113260af814333a56f59cb2b3" "bd242cdf786c47edd3be40e45bc95ac76e389d0cb1d3f0a586f4ca7acf2e2c89" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "0b3aee906629ac7c3bd994914bf252cf92f7a8b0baa6d94cb4dfacbd4068391d" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "1df2d767cc1b5ed78626f93f06c24ac15144a28b7420364769bf63cd23e420d3" "303cfaa6ce6653d3299583f9f002107487860b701d314fff589b7df77263d5fd" "205e4d3eb91d528237d49db05bd83a4f4de9d6c0b965c1c7d68fb958c36a7b7c" "1d78d6d05d98ad5b95205670fe6022d15dabf8d131fe087752cc55df03d88595" "8feca8afd3492985094597385f6a36d1f62298d289827aaa0d8a62fe6889b33c" "ae88c445c558b7632fc2d72b7d4b8dfb9427ac06aa82faab8d760fff8b8f243c" "8f0a782ba26728fa692d35e82367235ec607d0c836e06bc39eb750ecc8e08258" "bf815eb0b3031589aa53b6e01c57fa31e6fd367286204d2c15b6c07173ac63dc" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "d916b686ba9f23a46ee9620c967f6039ca4ea0e682c1b9219450acee80e10e40" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(display-time-day-and-date nil)
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
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(hl-paren-colors '("#ecf0f1" "#ecf0f1" "#c0392b"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#282a36" "#57c7ff"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#282a36" "#5af78e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#282a36" "#848688"))
 '(linum-format 'dynamic)
 '(lsp-eldoc-enable-hover nil)
 '(lsp-go-analyses '(("unusedparams" . t) ("assign" . t) ("unusedwrite" . t)))
 '(lsp-go-hover-kind "FullDocumentation")
 '(lsp-go-use-gofumpt t)
 '(lsp-ui-doc-max-height 30)
 '(lsp-ui-doc-max-width 60)
 '(nrepl-message-colors
   '("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d"))
 '(objed-cursor-color "#ff5c57")
 '(package-selected-packages
   '(multiple-cursors mark-multiple restclient dumb-jump dump-jump block-nav mwim bm smartparens json-reformat yaml-mode expand-region webpaste selected ace-jump-mode tango-plus-theme spacemacs-theme centaur-tabs tao-theme vscode-light-plus-theme github-theme github-modern-theme flatui-theme projectile goto-line-preview goto-line-previw doom-modeline tabbar magit git-emacs git highlight-indent-guides highlight-indentation highlight-indents vs-light-theme intellij-theme flycheck-golangci-lint js3-mode poly-markdown xref-js2 js2-refactor js2-mode json-mode multi-web-mode lsp-python-ms protobuf-mode web-mode go-mode company flycheck lsp-ui lsp-mode doom-themes neotree all-the-icons-dired yasnippet-snippets yasnippet use-package))
 '(pdf-view-midnight-colors (cons "#f9f9f9" "#282a36"))
 '(rustic-ansi-faces
   ["#282a36" "#ff5c57" "#5af78e" "#f3f99d" "#57c7ff" "#ff6ac1" "#9aedfe" "#f9f9f9"])
 '(show-paren-mode t)
 '(show-paren-style 'parenthesis)
 '(size-indication-mode nil)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
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
 '(vc-annotate-very-old-color nil)
 '(webpaste-return-url-hook nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t nil)))
 '(show-paren-match ((t (:foreground "white" :weight bold))))
 '(tab-bar ((t (:background "#1e1e1e" :weight normal :width normal)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
