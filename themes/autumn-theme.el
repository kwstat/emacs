;; autumn-theme.el
;; A variation of zenburn with dark brown background.  Inspired by colors of autumn.

;; MIT license. Copyright (c) Kevin Wright.

(deftheme autumn "The colors of autumn (dark brown background).")

(let ((gray-9   "#eeeeee")
      (gray-8   "#dddddd")
      (gray-7   "#bbbbbb")
      (gray-6   "#999999")
      (gray-5   "#777777")
      (gray-4   "#555555")
      (gray-3   "#444444")
      (gray-2   "#333333") ; zen background
      (gray-1   "#222222")
      (gray-0   "#111111")
      (red-5    "#f0c0d0") ; pink = #ffc0b0
      (red-4    "#d797a0")
      (red-3    "#c07070")
      (red-2    "#a74040")
      (red-1    "#901010")
      (red-0    "#500000") ; Others: magenta
      (orange-5 "#ffd080")
      (orange-4 "#ffc040")
      (orange-3 "#f0a720")
      (orange-2 "#e09000") ; peru = #cd853f
      (orange-1 "#ff7000") ; darkorange = #ff8c00
      (orange-0 "#b03000")
      (brown-3  "#706050")
      (brown-2  "#605040")
      (brown-1  "#504030") ; autumn background
      (brown-0  "#302010")
      (yellow-5 "#fafad2") ; lightgoldenrod = summer background
      (yellow-4 "#f0f090")
      (yellow-3 "#e6e66a")
      (yellow-2 "#dcdc37")
      (yellow-1 "#f9f900") ; yellow
      (yellow-0 "#b8860b") ; darkgoldenrod
      (green-5  "#dbf7c0")
      (green-4  "#acd595")
      (green-3  "#7eb26a")
      (green-2  "#509040")
      (green-1  "#006000") ; darkgreen = #006400
      (green-0  "#004000") ; Others: olivedrab = #6b8e23 lime "#b0f010"
      (cyan-2   "#90f0f0")
      (cyan-1   "#00ffff") ; cyan
      (blue-5   "#cee7ff")
      (blue-4   "#93b4d4")
      (blue-3   "#5982aa")
      (blue-2   "#205080")
      (blue-1   "#0000f0") ; blue
      (blue-0   "#000060")
      (purple-5 "#ecd9ff")
      (purple-4 "#ddbbff")
      (purple-3 "#bb99dd")
      (purple-2 "#9977bb")
      (purple-1 "#8000a0") ; purple
      (purple-0 "#400050")
      (tan-5    "#fff0c0") ; cornsilk = #fff8dc
      (tan-4    "#ead0a0") ; wheat = #f5deb3
      (tan-3    "#d5b080")
      (tan-2    "#c09060") ; peru = #cd853f
      (tan-1    "#947249")
      (tan-0    "#573700") ; saddlebrown = #8b4513
      (class '((class color) (min-colors 89)))
      (ol1 '(:foreground "#f0c0d0" :background "#500000" :height 1.3 :weight bold :overline "#f0c0d0"))
      (ol2 '(:foreground "#cee7ff" :background "#000060" :weight bold))
      (ol3 '(:foreground "#dbf7c0" :background "#004000" :weight bold))
      (ol4 '(:foreground "#fff0c0" :background "#573700" :weight bold))
      )

  (custom-theme-set-faces
   'autumn

   ;; Basic UI
   `(default ((t (:foreground ,tan-5 :background ,tan-0 ))))
   `(button ((t (:foreground ,blue-4 :underline t))))
   `(cursor ((t (:foreground ,brown-0 :background ,yellow-3))))
   `(fringe ((t (:foreground , yellow-3 :background ,brown-2))))
   `(minibuffer-prompt ((t (:foreground ,yellow-3 :background ,brown-0))))
   `(mode-line ((t (:foreground ,tan-0 :background ,tan-3
                                :box (:line-width 1 :color ,tan-4 :style released-button)))))
   `(mode-line-inactive ((t (:foreground ,tan-4 :background ,tan-1))))
   `(mode-line-buffer-id ((t (:foreground ,yellow-3 :weight bold))))
   `(region ((t (:background ,brown-0))))

   ;; comint
   `(comint-highlight-prompt ((t (:foreground ,blue-4 :weight bold))))

   ;; ediff  There are A and B buffers/files
   ;; current-diff-A is for all lines of the active region in A
   ;; fine-diff-A is the portion of region in A with actual differences
   ;; odd-diff-A even-diff-A  Non-active regions alternate these colors
   `(ediff-current-diff-A ((t (:foreground ,yellow-3 :background ,gray-0 ))))
   `(ediff-fine-diff-A ((t (:foreground ,green-4 :background ,green-0))))
   `(ediff-current-diff-B ((t (:foreground ,yellow-3 :background ,gray-0))))
   `(ediff-fine-diff-B ((t (:foreground ,blue-3 :background ,blue-0 ))))
   `(ediff-odd-diff-A ((t (:foreground ,yellow-3 :background ,brown-0))))
   `(ediff-even-diff-A ((t (:foreground ,yellow-3 :background ,brown-0))))
   `(ediff-odd-diff-B ((t (:foreground ,yellow-3 :background ,brown-0))))
   `(ediff-even-diff-B ((t (:foreground ,yellow-3 :background ,brown-0))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,yellow-3 :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,red-4 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,blue-2 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,red-3 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,yellow-3))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,yellow-3 :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,green-4 :weight bold))))

   ;; ess
   `(ess-debug-current-debug-line-face ((t (:background ,brown-0)))) ; region
   
   ;; flyspell
   `(flyspell-incorrect ((t (:inherit unspecified :underline "#ff7bbb"))))
   `(flyspell-duplicate ((t (:inherit unspecified :underline ,orange-2))))

   ;; font-lock
   `(font-lock-builtin-face ((t (:foreground ,blue-4))))
   `(font-lock-comment-face ((t (:foreground ,green-5))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,green-5))))
   `(font-lock-constant-face ((t (:foreground ,yellow-2))))
   `(font-lock-doc-face ((t (:foreground ,gray-7))))
   `(font-lock-doc-string-face ((t (:foreground ,blue-2))))
   `(font-lock-function-name-face ((t (:foreground ,red-5))))
   `(font-lock-keyword-face ((t (:foreground ,purple-4 :bold t))))
   `(font-lock-string-face ((t (:foreground ,red-5))))
   `(font-lock-type-face ((t (:foreground ,orange-3 :bold t ))))
   `(font-lock-variable-name-face ((t (:foreground ,orange-4))))
   `(font-lock-warning-face ((t (:foreground ,red-4 :background ,red-0))))

   ;;; grep
   `(grep-context-face ((t (:foreground ,gray-9 ))))
   `(grep-error-face ((t (:foreground ,red-4 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,blue-2))))
   `(grep-match-face ((t (:foreground ,orange-3 :weight bold))))
   `(match ((t (:background ,brown-0 :foreground ,orange-3 :weight bold))))

   ;; ido
   `(ido-first-match ((t (:foreground ,green-4 :background ,green-0 :weight bold))))
   `(ido-only-match ((t (:foreground ,green-4 :background ,green-0 :weight bold))))
   `(ido-subdir ((t (:foreground ,red-4))))

   ;; isearch
   `(isearch ((t (:foreground ,green-4 :background ,green-0)))) ; first match
   `(lazy-highlight ((t (:foreground ,blue-4 :background ,blue-0)))) ; other matches
   `(isearch-fail ((t (:foreground ,red-4 :background ,red-0))))

   ;; misc
   `(hl-line ((t (:background ,blue-0))))
   `(custom-link ((t (:foreground ,blue-3 :underline t))))

   ;; latex
   `(font-latex-sedate-face ((t (:foreground ,orange-5))))
   `(font-latex-warning-face ((t (:foreground ,orange-1))))

   ;; markdown
   `(markdown-header-delimiter-face ((t (:foreground ,gray-8 :weight bold))))
   `(markdown-metadata-value-face  ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "black"))))
   `(markdown-header-face-1 ((,class ,ol1)))
   `(markdown-header-face-2 ((,class ,ol2)))
   `(markdown-header-face-3 ((,class ,ol3)))
   `(markdown-header-face-4 ((,class ,ol4)))
 
   ;; org
   `(org-date ((t (:foreground ,purple-4))))
   `(org-document-title ((t (:height 1.6 :weight bold))))
   `(org-done ((t (:foreground ,green-4))))
   `(org-hide ((t (:foreground ,blue-0))))
   `(org-link ((t (:foreground ,blue-5))))
   `(org-todo ((t (:foreground ,red-3))))
   `(org-level-1 ((,class ,ol1)))
   `(org-level-2 ((,class ,ol2)))
   `(org-level-3 ((,class ,ol3)))
   `(org-level-4 ((,class ,ol4)))

   ;; paren
   `(show-paren-match ((t (:foreground ,blue-0 :background ,blue-3))))
   `(show-paren-mismatch ((t (:foreground ,red-0 :background ,red-4))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,green-4))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,yellow-3))))
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,blue-3))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,red-4))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,orange-4))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,purple-4))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,green-2))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,yellow-1))))
   `(rainbow-delimiters-depth-0-face ((t (:foreground ,blue-1))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,red-2))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,orange-2))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,purple-2))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,red-4 :background ,red-0))))

   ;; which
   `(which-func ((t (:foreground ,orange-4))))

)

  (custom-theme-set-variables
   'autumn

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,gray-0 ,red-4 ,green-4 ,yellow-3 ,blue-3 ,purple-4 ,cyan-2, "white"])
   )
  )

(provide-theme 'autumn)
