(toggle-frame-fullscreen)
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(require 'subr-x)
(require 'ibuffer)
(setq-default ibuffer-use-header-line nil)
(setq-default ibuffer-default-sorting-mode 'alphabetic)

(add-hook 'ibuffer-hook
		  (lambda ()
			  (if (not default-buffers-shown)
				  (ibuffer-filter-by-name "^[^\*]"))
			  (ibuffer-update)))

(load-file "~/.emacs.d/init_window.el")
(load-file "~/.emacs.d/custom.el")
(load-file "~/.emacs.d/remaps.el")
(load-file "~/.emacs.d/keybindings.el")

(add-to-list 'default-frame-alist '(left . 20))
(add-to-list 'default-frame-alist '(top . 20))
(add-to-list 'default-frame-alist '(height . 30))
(add-to-list 'default-frame-alist '(width . 120))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :italic t)))
 '(cursor ((t (:box (:line-width (2 . 2) :color "#ccc" :style released-button)))))
 '(diff-refine-added ((t (:inherit diff-added :background "red" :inverse-video t))))
 '(magit-diff-added ((t (:extend t :background "red" :foreground "#6b8d6c"))))
 '(magit-diff-added-highlight ((t (:extend t :background "red" :foreground "#86B187" :weight bold)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(git-gutter blamer ujelly-theme yasnippet-snippets yaml-mode webpaste web-mode vue-mode vscode-dark-plus-theme vlf vdiff-magit use-package typescript-mode sublimity sqlup-mode sql-indent solarized-theme smart-shift selected restclient rainbow-mode protobuf-mode powerline org-jira no-littering neotree multiple-cursors multi-web-mode minimap mark-multiple magit-delta lsp-ui key-chord json-mode jq-format jenkinsfile-mode jenkins-watch jenkins intellij-theme ibuffer-sidebar ibuffer-projectile ibuffer-project html-to-markdown highlight-indent-guides hide-mode-line helm harvest graphql-mode goto-line-preview go-mode gitlab git-lens git friendly-shell-command friendly-shell format-all fold-dwim flycheck-golangci-lint expand-region elisp-format eglot doom-themes doom-modeline dockerfile-mode diffview dap-mode dakrone-theme company-tabnine bm apheleia all-the-icons-ibuffer all-the-icons-dired ace-jump-mode)))
