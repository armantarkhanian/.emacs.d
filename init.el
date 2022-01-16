(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(add-hook 'buffer-list-update-hook (lambda ()
									   (if (not (active-minibuffer-window))
										   (setq header-line-format (generateHeadline))
										   )))


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
