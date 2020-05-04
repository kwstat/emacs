;; summer-theme.el
;; A dark-on-light-yellow theme.

;; MIT license. Copyright (c) Kevin Wright.

(deftheme summer "The colors of summer (light yellow background).")

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
      (green-0  "#004000") ; Others: olivedrab = #6b8e23 lime "#b0f010" olive
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
      (ol1 '(:foreground "#901010" :background "#f0c0d0" :height 1.3 :weight bold))
      (ol2 '(:foreground "#0000f0" :background "#cee7ff" :weight bold))
      (ol3 '(:foreground "#006000" :background "#dbf7c0" :weight bold))
      (ol4 '(:foreground "#573700" :background "#fff0c0" :weight bold))

      )

(custom-theme-set-faces
 'summer

 ;;; Basic UI
 `(default ((t (:foreground ,brown-0 :background "#fdfde7")))) ; was yellow-5
 `(button ((t (:foreground ,blue-1 :underline t))))
 `(cursor ((t (:foreground ,gray-8 :background ,gray-2))))
 `(fringe ((t (:foreground, yellow-3 :background ,yellow-5))))
 `(minibuffer-prompt ((t (:foreground ,tan-5 :background ,tan-0 ))))
 `(mode-line ((t (:foreground ,tan-0 :background ,yellow-2
                              :box (:line-width 2 :color ,yellow-3 :style released-button)))))
 `(mode-line-inactive ((t (:foreground ,tan-0 :background ,gray-8)))) ; yellow-4
 `(region ((t (:background ,yellow-4))))

 ;; comint
 `(comint-highlight-prompt ((t (:foreground ,blue-1 :weight bold))))

 ;; ess
 `(ess-debug-current-debug-line-face ((t (:background ,yellow-4)))) ; region
   
 ;; flyspell
 `(flyspell-incorrect ((t (:inherit unspecified :underline "red"))))
 `(flyspell-duplicate ((t (:inherit unspecified :underline ,orange-5))))

 ;; font-lock
 `(font-lock-builtin-face ((t (:foreground , blue-2))))
 `(font-lock-comment-face ((t (:foreground ,tan-1 :italic t))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,tan-1))))
 `(font-lock-constant-face ((t (:foreground ,green-1))))
 `(font-lock-doc-face ((t (:foreground ,red-0))))
 `(font-lock-doc-string-face ((t (:foreground "#7285b7"))))
 `(font-lock-function-name-face ((t (:foreground ,blue-0))))
 `(font-lock-keyword-face ((t (:foreground ,purple-1 :weight bold))))
 `(font-lock-string-face ((t (:foreground ,green-1 :weight bold))))
 `(font-lock-type-face ((t (:foreground ,orange-1 :weight bold))))
 `(font-lock-variable-name-face ((t (:foreground "saddlebrown" :weight bold))))
 `(font-lock-warning-face ((t (:foreground ,red-1 :weight bold))))
 `(hl-line ((t (:background "#b5ba99"))))
 `(linum ((t (:background "peru" :foreground "#ffffff"))))

; `(org-date ((t (:foreground "#ebbbff"))))
 `(org-document-title ((t ( :height 1.6 :weight bold))))
 `(org-done ((t (:foreground "#d1f1a9"))))
 `(org-hide ((t (:foreground "#00346e"))))
 `(org-link ((t (:foreground "#1144aa"))))
 `(org-todo ((t (:foreground "#ff9da4"))))
 `(org-level-1 ((,class ,ol1)))
 `(org-level-2 ((,class ,ol2)))
 `(org-level-3 ((,class ,ol3)))
 `(org-level-4 ((,class ,ol4)))
 `(org-block-begin-line ((t (:foreground ,tan-0 :background ,tan-5 :weight bold))))
 `(org-block-end-line ((t (:foreground ,tan-0 :background ,tan-5 :weight bold))))

 ;; ido
 `(ido-first-match ((t (:foreground ,green-0 :background ,green-5 :weight bold))))
 `(ido-only-match ((t (:foreground ,green-0 :background ,green-5 :weight bold))))
 `(ido-subdir ((t (:foreground ,red-0))))

 ;; isearch
 `(isearch ((t (:foreground ,green-0 :background ,green-5)))) ; first match
 `(lazy-highlight ((t (:foreground ,blue-0 :background ,blue-5)))) ; other matches
 `(isearch-fail ((t (:foreground ,red-0 :background ,red-5))))

 ;; latex
 `(font-latex-sedate-face ((t (:foreground ,orange-2))))
 `(font-latex-warning-face ((t (:foreground ,red-1))))
 `(markdown-header-delimiter-face ((t (:foreground ,gray-3 :weight bold))))
 `(markdown-metadata-value-face  ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "black"))))
 `(markdown-header-face-1 ((,class ,ol1)))
 `(markdown-header-face-2 ((,class ,ol2)))
 `(markdown-header-face-3 ((,class ,ol3)))
 `(markdown-header-face-4 ((,class ,ol4)))
; `(markdown-header-rule-face ((,class ,ol4)))

 ;; paren
 `(show-paren-match ((t (:background "#bbdaff" :foreground "#00346e"))))
 `(show-paren-mismatch ((t (:background "#ffc58f" :foreground "#00346e"))))

 ;; rainbow-delimiters
 `(rainbow-delimiters-depth-1-face ((t (:foreground ,gray-1 :weight bold))))
 `(rainbow-delimiters-depth-2-face ((t (:foreground ,red-1 :weight bold))))
 `(rainbow-delimiters-depth-3-face ((t (:foreground ,blue-1 :weight bold))))
 `(rainbow-delimiters-depth-4-face ((t (:foreground ,green-1 :weight bold))))
 `(rainbow-delimiters-depth-5-face ((t (:foreground ,orange-1 :weight bold))))
 `(rainbow-delimiters-depth-6-face ((t (:foreground ,purple-1 :weight bold))))
 `(rainbow-delimiters-depth-7-face ((t (:foreground ,tan-1 :weight bold))))
 `(rainbow-delimiters-depth-8-face ((t (:foreground ,blue-3 :weight bold))))
 `(rainbow-delimiters-depth-9-face ((t (:foreground ,red-4 :weight bold))))
 `(rainbow-delimiters-unmatched-face ((t (:foreground ,red-5 :background ,red-0))))

)


  (custom-theme-set-variables
   'summer

 '(ansi-color-names-vector ["#00346e" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#bbdaff" "#ffffff"]))

)

(provide-theme 'summer)
