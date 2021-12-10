(require 'subr-x)
(require 'ibuffer)
(setq-default ibuffer-use-header-line nil)
(setq-default ibuffer-default-sorting-mode 'alphabetic)

(add-hook 'ibuffer-hook
          (lambda ()
              (if (not default-buffers-shown)
                  (ibuffer-filter-by-name "^[^\*]"))
              (ibuffer-update)))

(if (display-graphic-p)
    (load "~/.emacs.d/init_window.el")
    (load "~/.emacs.d/init_window.el")
    ;;(load "~/.emacs.d/init_nw.el"))
    )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#191919" "#FF5E5E" "#468800" "#E9FDAC" "#8CDAFF" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("cf861f5603b7d22cb3545a7c63b2ee424c34d8ed3b3aa52d13abfea4765cffe7" "2f1ed0c737c498295c7b72396cb9c168556b7bfc637967ec2fba044b63d55ece" "9c45a0351e89fe6e950ed7ff7c7bf785344dcb56598e6d97532c638c90000309" "665fbb8e59d19f13bb1f1215af8180758b464eefe239b9a59ccd57486958b68b" "62eca1d5615ff9695691704030914c470e6a6c0244d5049893715bcbd202c0dd" "8543bb312515c9e477f629a729c2bf8a4f71332c5756f32f3cb5fbbd0a0a785a" "303cfaa6ce6653d3299583f9f002107487860b701d314fff589b7df77263d5fd" "98ef70f6028d10db0a3e1178b741df39c2dfefad97eda6ff1f17d5e83fe32179" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "f4c8f0b999a6407211a899401315a628e1a5ae2f408c04a33b14d7aa3ed86187" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" default))
 '(display-time-day-and-date nil)
 '(eldoc-idle-delay 0.0)
 '(exwm-floating-border-color "#121212")
 '(fci-rule-color "#515151")
 '(highlight-tail-colors ((("#1d2416" "#1d2416") . 0) (("#232c30" "#202c30") . 20)))
 '(ibuffer-sidebar-refresh-timer 0)
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#FFFFFF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#468800"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(linum-format 'dynamic)
 '(lsp-eldoc-enable-hover nil)
 '(neo-mode-line-type 'none)
 '(nil nil t)
 '(objed-cursor-color "#FF5E5E")
 '(package-selected-packages
   '(friendly-shell-command friendly-shell json-mode jq-format mu awesome-tab eglot ibuffer-projectile ibuffer-project all-the-icons-ibuffer diffview vdiff-magit vdiff no-littering edit-indirect helm graphql-mode dap-mode go-dlv indent-tools sql-sqlline sqlup-mode sql-smie format-sql reformatter sql-indent sqlformat html-to-markdown typescript-mode dockerfile-mode key-chord ace-window rainbow-mode elisp-format vscdark-theme yasnippet-snippets yaml-mode wttrin webpaste web-mode vue-mode vscode-dark-plus-theme vs-light-theme use-package tao-theme tango-plus-theme tabbar spacemacs-theme smartparens selected restclient protobuf-mode projectile nord-theme neotree mwim multiple-cursors multi-web-mode mark-multiple magit lsp-ui lsp-python-ms light-soap-theme json-reformat jetbrains-darcula-theme intellij-theme ibuffer-sidebar highlight-indentation highlight-indent-guides goto-line-preview google-translate google go-mode github-theme github-modern-theme git format-all flycheck-golangci-lint flatui-theme expand-region espresso-theme epc dumb-jump doom-themes doom-modeline dashboard company centaur-tabs bm block-nav avy atom-one-dark-theme all-the-icons-dired afternoon-theme ace-jump-mode))
 '(pdf-view-midnight-colors (cons "#d4d4d4" "#191919"))
 '(resize-mini-windows nil)
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
