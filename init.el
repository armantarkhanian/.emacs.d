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
 '(mc/max-cursors 10))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blamer-face ((t :foreground "#7a88cf" :background nil :italic t)))
 '(cursor ((t (:background "#8AC6F2")))))
