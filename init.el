(require 'subr-x)
(require 'ibuffer)
(setq-default ibuffer-use-header-line nil)
(setq-default ibuffer-default-sorting-mode 'alphabetic)

(add-hook 'ibuffer-hook
          (lambda ()
              (if (not default-buffers-shown)
                  (ibuffer-filter-by-name "^[^\*]"))
              (ibuffer-update)))

(load "~/.emacs.d/init_window.el")

(load "~/.emacs.d/custom.el")
(load "~/.emacs.d/remaps.el")
(load "~/.emacs.d/keybindings.el")

(set-frame-size (selected-frame) 100 30)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "d916b686ba9f23a46ee9620c967f6039ca4ea0e682c1b9219450acee80e10e40" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "c5ded9320a346146bbc2ead692f0c63be512747963257f18cc8518c5254b7bf5" "846b3dc12d774794861d81d7d2dcdb9645f82423565bfb4dad01204fa322dbd5" "d6844d1e698d76ef048a53cefe713dbbe3af43a1362de81cdd3aefa3711eae0d" "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "850bb46cc41d8a28669f78b98db04a46053eca663db71a001b40288a9b36796c" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "b5803dfb0e4b6b71f309606587dd88651efe0972a5be16ece6a958b197caeed8" "23c806e34594a583ea5bbf5adf9a964afe4f28b4467d28777bcba0d35aa0872e" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "1f1b545575c81b967879a5dddc878783e6ebcca764e4916a270f9474215289e5" "a82ab9f1308b4e10684815b08c9cac6b07d5ccb12491f44a942d845b406b0296" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "97db542a8a1731ef44b60bc97406c1eb7ed4528b0d7296997cbb53969df852d6" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "d47f868fd34613bd1fc11721fe055f26fd163426a299d45ce69bef1f109e1e71" "f7fed1aadf1967523c120c4c82ea48442a51ac65074ba544a5aefc5af490893b" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "4b6b6b0a44a40f3586f0f641c25340718c7c626cbf163a78b5a399fbe0226659" "266ecb1511fa3513ed7992e6cd461756a895dcc5fef2d378f165fed1c894a78c" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "4133d2d6553fe5af2ce3f24b7267af475b5e839069ba0e5c80416aa28913e89a" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "1278c5f263cdb064b5c86ab7aa0a76552082cf0189acf6df17269219ba496053" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7" "b186688fbec5e00ee8683b9f2588523abdf2db40562839b2c5458fcfb322c8a4" "3d54650e34fa27561eb81fc3ceed504970cc553cfd37f46e8a80ec32254a3ec3" "6c98bc9f39e8f8fd6da5b9c74a624cbb3782b4be8abae8fd84cbc43053d7c175" "e2c926ced58e48afc87f4415af9b7f7b58e62ec792659fcb626e8cba674d2065" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "cf861f5603b7d22cb3545a7c63b2ee424c34d8ed3b3aa52d13abfea4765cffe7" "2f1ed0c737c498295c7b72396cb9c168556b7bfc637967ec2fba044b63d55ece" "9c45a0351e89fe6e950ed7ff7c7bf785344dcb56598e6d97532c638c90000309" "665fbb8e59d19f13bb1f1215af8180758b464eefe239b9a59ccd57486958b68b" "62eca1d5615ff9695691704030914c470e6a6c0244d5049893715bcbd202c0dd" "8543bb312515c9e477f629a729c2bf8a4f71332c5756f32f3cb5fbbd0a0a785a" "303cfaa6ce6653d3299583f9f002107487860b701d314fff589b7df77263d5fd" "98ef70f6028d10db0a3e1178b741df39c2dfefad97eda6ff1f17d5e83fe32179" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "f4c8f0b999a6407211a899401315a628e1a5ae2f408c04a33b14d7aa3ed86187" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" default))
 '(display-time-day-and-date nil)
 '(ediff-split-window-function 'split-window-horizontally)
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
 '(max-mini-window-height 0)
 '(neo-mode-line-type 'none)
 '(nil nil t)
 '(objed-cursor-color "#FF5E5E")
 '(package-selected-packages
   '(solarized-theme magit-delta vlf apheleia company-tabnine sublimity minimap hide-mode-line git-lens gitlab harvest org-jira ejira fold-dwim smart-shift friendly-shell-command friendly-shell json-mode jq-format mu awesome-tab eglot ibuffer-projectile ibuffer-project all-the-icons-ibuffer diffview vdiff-magit vdiff no-littering edit-indirect helm graphql-mode dap-mode go-dlv indent-tools sql-sqlline sqlup-mode sql-smie format-sql reformatter sql-indent sqlformat html-to-markdown typescript-mode dockerfile-mode key-chord ace-window rainbow-mode elisp-format vscdark-theme yasnippet-snippets yaml-mode wttrin webpaste web-mode vue-mode vscode-dark-plus-theme vs-light-theme use-package tao-theme tango-plus-theme tabbar spacemacs-theme smartparens selected restclient protobuf-mode projectile nord-theme neotree mwim multiple-cursors multi-web-mode mark-multiple magit lsp-ui lsp-python-ms light-soap-theme json-reformat jetbrains-darcula-theme intellij-theme ibuffer-sidebar highlight-indentation highlight-indent-guides goto-line-preview google-translate google go-mode github-theme github-modern-theme git format-all flycheck-golangci-lint flatui-theme expand-region espresso-theme epc dumb-jump doom-themes doom-modeline dashboard company centaur-tabs bm block-nav avy atom-one-dark-theme all-the-icons-dired afternoon-theme ace-jump-mode))
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
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#171717" :foreground "#F6F3E8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 113 :width normal :foundry "PfEd" :family "Monospace")))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
