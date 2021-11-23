(setq warning-minimum-level
      :emergency)

(load-theme 'wombat)

(load "~/.emacs.d/custom.el")
(load "~/.emacs.d/keybindings.el")

(show-paren-mode 1)
(global-hl-line-mode 1)
(global-display-line-numbers-mode)
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

(add-hook 'yaml-mode-hook (lambda ()
                              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

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
