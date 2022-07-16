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
(setq ibuffer-use-header-line nil)
(setq ibuffer-default-sorting-mode 'alphabetic)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mc/list-file "/home/arman/.emacs.d/var/mc-list.el")
 '(mc/max-cursors 20)
 '(package-selected-packages
   '(clues-theme afternoon-theme crdt sqlformat pandoc pandoc-mode nginx-mode dired-sidebar go-dlv eslint-fix kotlin-mode ccls lsp-javacomp lsp-java yasnippet-snippets yaml-mode webpaste web-mode vue-mode vscode-dark-plus-theme vlf vdiff-magit use-package ujelly-theme typescript-mode sublimity sqlup-mode sql-indent solarized-theme smart-shift selected restclient rainbow-mode protobuf-mode powerline org-jira no-littering neotree multiple-cursors multi-web-mode minimap mark-multiple magit-delta lsp-ui key-chord json-mode jq-format jenkinsfile-mode jenkins-watch jenkins intellij-theme ibuffer-sidebar ibuffer-projectile ibuffer-project html-to-markdown highlight-indent-guides hide-mode-line helm harvest graphql-mode goto-line-preview go-mode gitlab github-theme git-lens git-gutter git friendly-shell-command friendly-shell format-all fold-dwim flycheck-golangci-lint expand-region elisp-format eglot doom-themes doom-modeline dockerfile-mode diffview dap-mode dakrone-theme company-tabnine bm blamer apheleia all-the-icons-ibuffer all-the-icons-dired ace-jump-mode))
 '(tabbar-separator '(0.5)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :italic t)))
 '(cursor ((t (:background "#8AC6F2")))))
