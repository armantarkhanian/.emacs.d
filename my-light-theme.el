(deftheme my-light "Created 2021-08-31.")

(custom-theme-set-faces 'my-light
                        '(default ((t
                                    (:family "DejaVu Sans Mono"
                                             :foundry "PfEd"
                                             :width normal
                                             :height 113
                                             :weight normal
                                             :slant normal
                                             :underline nil
                                             :overline nil
                                             :extend nil
                                             :strike-through nil
                                             :box nil
                                             :inverse-video nil
                                             :foreground "black"
                                             :background "white"
                                             :stipple nil
                                             :inherit nil))))

                        '(cursor ((((background light))
                                   (:background "black"))
                                  (((background dark))
                                   (:background "white"))))

                        '(fixed-pitch ((t
                                        (:family "Monospace"))))

                        '(variable-pitch ((((type w32))
                                           (:foundry "outline"
                                                     :family "Arial"))
                                          (t
                                           (:family "Sans Serif"))))

                        '(escape-glyph ((((background dark))
                                         (:foreground "cyan"))
                                        (((type pc))
                                         (:foreground "magenta"))
                                        (t
                                         (:foreground "brown"))))

                        '(homoglyph ((((background dark))
                                      (:foreground "cyan"))
                                     (((type pc))
                                      (:foreground "magenta"))
                                     (t
                                      (:foreground "brown"))))

                        '(minibuffer-prompt ((((background dark))
                                              (:foreground "cyan"))
                                             (((type pc))
                                              (:foreground "magenta"))
                                             (t
                                              (:foreground "medium blue"))))

                        '(highlight ((t
                                      (:background "#f1f1f1"))))

                        '(region ((((class color)
                                    (min-colors 88)
                                    (background dark))
                                   (:extend t
                                            :background "blue3"))
                                  (((class color)
                                    (min-colors 88)
                                    (background light)
                                    (type gtk))
                                   (:extend t
                                            :background "gtk_selection_bg_color"
                                            :distant-foreground "gtk_selection_fg_color"))
                                  (((class color)
                                    (min-colors 88)
                                    (background light)
                                    (type ns))
                                   (:extend t
                                            :background "ns_selection_bg_color"
                                            :distant-foreground "ns_selection_fg_color"))
                                  (((class color)
                                    (min-colors 88)
                                    (background light))
                                   (:extend t
                                            :background "lightgoldenrod2"))
                                  (((class color)
                                    (min-colors 16)
                                    (background dark))
                                   (:extend t
                                            :background "blue3"))
                                  (((class color)
                                    (min-colors 16)
                                    (background light))
                                   (:extend t
                                            :background "lightgoldenrod2"))
                                  (((class color)
                                    (min-colors 8))
                                   (:extend t
                                            :foreground "white"
                                            :background "blue"))
                                  (((type tty)
                                    (class mono))
                                   (:inverse-video t))
                                  (t
                                   (:extend t
                                            :background "gray"))))

                        '(shadow ((((class color grayscale)
                                    (min-colors 88)
                                    (background light))
                                   (:foreground "grey50"))
                                  (((class color grayscale)
                                    (min-colors 88)
                                    (background dark))
                                   (:foreground "grey70"))
                                  (((class color)
                                    (min-colors 8)
                                    (background light))
                                   (:foreground "green"))
                                  (((class color)
                                    (min-colors 8)
                                    (background dark))
                                   (:foreground "yellow"))))

                        '(secondary-selection ((((class color)
                                                 (min-colors 88)
                                                 (background light))
                                                (:extend t
                                                         :background "yellow1"))
                                               (((class color)
                                                 (min-colors 88)
                                                 (background dark))
                                                (:extend t
                                                         :background "SkyBlue4"))
                                               (((class color)
                                                 (min-colors 16)
                                                 (background light))
                                                (:extend t
                                                         :background "yellow"))
                                               (((class color)
                                                 (min-colors 16)
                                                 (background dark))
                                                (:extend t
                                                         :background "SkyBlue4"))
                                               (((class color)
                                                 (min-colors 8))
                                                (:extend t
                                                         :foreground "black"
                                                         :background "cyan"))
                                               (t
                                                (:inverse-video t))))

                        '(trailing-whitespace ((((class color)
                                                 (background light))
                                                (:background "red1"))
                                               (((class color)
                                                 (background dark))
                                                (:background "red1"))
                                               (t
                                                (:inverse-video t))))

                        '(font-lock-builtin-face ((((class grayscale)
                                                    (background light))
                                                   (:weight bold
                                                            :foreground "LightGray"))
                                                  (((class grayscale)
                                                    (background dark))
                                                   (:weight bold
                                                            :foreground "DimGray"))
                                                  (((class color)
                                                    (min-colors 88)
                                                    (background light))
                                                   (:foreground "dark slate blue"))
                                                  (((class color)
                                                    (min-colors 88)
                                                    (background dark))
                                                   (:foreground "LightSteelBlue"))
                                                  (((class color)
                                                    (min-colors 16)
                                                    (background light))
                                                   (:foreground "Orchid"))
                                                  (((class color)
                                                    (min-colors 16)
                                                    (background dark))
                                                   (:foreground "LightSteelBlue"))
                                                  (((class color)
                                                    (min-colors 8))
                                                   (:weight bold
                                                            :foreground "blue"))
                                                  (t
                                                   (:weight bold))))

                        '(font-lock-comment-delimiter-face ((t
                                                             (:inherit font-lock-comment-face))))

                        '(font-lock-comment-face ((t
                                                   (:foreground "#008000"))))

                        '(font-lock-constant-face ((t
                                                    (:foreground "#0000ff"))))

                        '(font-lock-doc-face ((t
                                               (:inherit (font-lock-string-face)))))

                        '(font-lock-function-name-face ((t
                                                         (:foreground "#795E26"))))

                        '(font-lock-keyword-face ((t
                                                   (:foreground "#0000ff"))))

                        '(font-lock-negation-char-face ((t nil)))

                        '(font-lock-preprocessor-face ((t
                                                        (:inherit (font-lock-builtin-face)))))

                        '(font-lock-regexp-grouping-backslash ((t
                                                                (:inherit (bold)))))

                        '(font-lock-regexp-grouping-construct ((t
                                                                (:inherit (bold)))))

                        '(font-lock-string-face ((t
                                                  (:foreground "#a31515"))))

                        '(font-lock-type-face ((((class grayscale)
                                                 (background light))
                                                (:weight bold
                                                         :foreground "Gray90"))
                                               (((class grayscale)
                                                 (background dark))
                                                (:weight bold
                                                         :foreground "DimGray"))
                                               (((class color)
                                                 (min-colors 88)
                                                 (background light))
                                                (:foreground "ForestGreen"))
                                               (((class color)
                                                 (min-colors 88)
                                                 (background dark))
                                                (:foreground "PaleGreen"))
                                               (((class color)
                                                 (min-colors 16)
                                                 (background light))
                                                (:foreground "ForestGreen"))
                                               (((class color)
                                                 (min-colors 16)
                                                 (background dark))
                                                (:foreground "PaleGreen"))
                                               (((class color)
                                                 (min-colors 8))
                                                (:foreground "green"))
                                               (t
                                                (:underline (:color foreground-color
                                                                    :style line)
                                                            :weight bold))))

                        '(font-lock-variable-name-face ((((class grayscale)
                                                          (background light))
                                                         (:slant italic
                                                                 :weight bold
                                                                 :foreground "Gray90"))
                                                        (((class grayscale)
                                                          (background dark))
                                                         (:slant italic
                                                                 :weight bold
                                                                 :foreground "DimGray"))
                                                        (((class color)
                                                          (min-colors 88)
                                                          (background light))
                                                         (:foreground "sienna"))
                                                        (((class color)
                                                          (min-colors 88)
                                                          (background dark))
                                                         (:foreground "LightGoldenrod"))
                                                        (((class color)
                                                          (min-colors 16)
                                                          (background light))
                                                         (:foreground "DarkGoldenrod"))
                                                        (((class color)
                                                          (min-colors 16)
                                                          (background dark))
                                                         (:foreground "LightGoldenrod"))
                                                        (((class color)
                                                          (min-colors 8))
                                                         (:weight light
                                                                  :foreground "yellow"))
                                                        (t
                                                         (:slant italic
                                                                 :weight bold))))

                        '(font-lock-warning-face ((t
                                                   (:inherit (error)))))

                        '(button ((t
                                   (:inherit (link)))))

                        '(link ((t
                                 (:underline (:color foreground-color
                                                     :style line)
                                             :foreground "RoyalBlue3"))))

                        '(link-visited ((t
                                         (:foreground "magenta4"
                                                      :inherit (link)))))

                        '(fringe ((((class color)
                                    (background light))
                                   (:background "grey95"))
                                  (((class color)
                                    (background dark))
                                   (:background "grey10"))
                                  (t
                                   (:background "gray"))))

                        '(header-line ((t
                                        (:box nil
                                              :foreground "grey20"
                                              :background "grey90"
                                              :inherit (mode-line)))))

                        '(tooltip ((t
                                    (:foreground "black"
                                                 :background "lightyellow"
                                                 :inherit (variable-pitch)))))

                        '(mode-line ((t nil)))

                        '(mode-line-buffer-id ((t
                                                (:weight bold))))

                        '(mode-line-emphasis ((t
                                               (:weight bold))))

                        '(mode-line-highlight ((((class color)
                                                 (min-colors 88))
                                                (:box (:line-width 2
                                                                   :color "grey40"
                                                                   :style released-button)))
                                               (t
                                                (:inherit (highlight)))))

                        '(mode-line-inactive ((t
                                               (:weight light
                                                        :box (:line-width -1
                                                                          :color "grey75"
                                                                          :style nil)
                                                        :foreground "grey20"
                                                        :background "grey90"
                                                        :inherit (mode-line)))))

                        '(isearch ((((class color)
                                     (min-colors 88)
                                     (background light))
                                    (:foreground "lightskyblue1"
                                                 :background "magenta3"))
                                   (((class color)
                                     (min-colors 88)
                                     (background dark))
                                    (:foreground "brown4"
                                                 :background "palevioletred2"))
                                   (((class color)
                                     (min-colors 16))
                                    (:foreground "cyan1"
                                                 :background "magenta4"))
                                   (((class color)
                                     (min-colors 8))
                                    (:foreground "cyan1"
                                                 :background "magenta4"))
                                   (t
                                    (:inverse-video t))))

                        '(isearch-fail ((((class color)
                                          (min-colors 88)
                                          (background light))
                                         (:background "RosyBrown1"))
                                        (((class color)
                                          (min-colors 88)
                                          (background dark))
                                         (:background "red4"))
                                        (((class color)
                                          (min-colors 16))
                                         (:background "red"))
                                        (((class color)
                                          (min-colors 8))
                                         (:background "red"))
                                        (((class color grayscale))
                                         (:foreground "grey"))
                                        (t
                                         (:inverse-video t))))

                        '(lazy-highlight ((((class color)
                                            (min-colors 88)
                                            (background light))
                                           (:background "paleturquoise"))
                                          (((class color)
                                            (min-colors 88)
                                            (background dark))
                                           (:background "paleturquoise4"))
                                          (((class color)
                                            (min-colors 16))
                                           (:background "turquoise3"))
                                          (((class color)
                                            (min-colors 8))
                                           (:background "turquoise3"))
                                          (t
                                           (:underline (:color foreground-color
                                                               :style line)))))

                        '(match ((((class color)
                                   (min-colors 88)
                                   (background light))
                                  (:background "yellow1"))
                                 (((class color)
                                   (min-colors 88)
                                   (background dark))
                                  (:background "RoyalBlue3"))
                                 (((class color)
                                   (min-colors 8)
                                   (background light))
                                  (:foreground "black"
                                               :background "yellow"))
                                 (((class color)
                                   (min-colors 8)
                                   (background dark))
                                  (:foreground "white"
                                               :background "blue"))
                                 (((type tty)
                                   (class mono))
                                  (:inverse-video t))
                                 (t
                                  (:background "gray"))))

                        '(next-error ((t
                                       (:inherit (region)))))

                        '(query-replace ((t
                                          (:inherit (isearch))))))

(provide-theme 'my-light)
